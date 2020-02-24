{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.DesktopNotifications
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
-- https://developer.gnome.org/notification-spec/
-- https://hackage.haskell.org/package/fdo-notify-0.3.1/docs/DBus-Notify.html#t:Notification
-- https://developer.gnome.org/pango/stable/PangoMarkupFormat.html
--
------------------------------------------------------------------------------

module XMonad.Util.DesktopNotifications where

import           Prelude

import           Control.Concurrent          (forkIO)
import qualified Control.Concurrent.MVar     as MVar
import qualified Control.Exception           as E
import           Control.Monad

import           Data.Int                    (Int32)
import qualified Data.Map                    as M
import           Data.Maybe                  (isNothing)
import           Data.String                 (IsString(..))
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL
import           Data.Word                   (Word32, Word8)

import           Text.Printf (PrintfArg)

import           System.Directory            (doesFileExist)
import           System.IO                   (stderr)
import qualified System.IO
import           System.Timeout              (timeout)

import           DBus                        hiding (Type)
import qualified DBus.Client                 as C

import           XMonad                      (Default(..), ExtensionClass(..), MonadIO(liftIO), X)
import qualified XMonad                      as X
import qualified XMonad.StackSet             as W
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.Util.NamedWindows    as NW

-- $bodyMarkup
--
-- Fancier servers support full HTML with CSS in the notification body.
-- Lighter ones may only support plain text, though many support the minimum
-- tags from the spec:
--
-- bold @<b></b>@, italics @<i></i>@, underline @<u></u>@,
-- link @<a href="…"></a>@, image @<img src="…" alt="…"/>@.
--
-- If the "body-markup" capability is /not/ present, the server will be
-- displaying the body text as-is. If the capability is present, the server
-- will display or at least strip away markup tags.
--
-- Finally some are somewhere in the middle with extended markup support to
-- that of the minimum. Servers using the Pango library for rendering are
-- likely to support its text markup format.
--
-- https://developer.gnome.org/pango/stable/PangoMarkupFormat.html
--
-- XML '&' escape codes may or may not be understood. Pango markup and heavier
-- HTML-based ones do understand them.

-- * Constants

object :: ObjectPath
object = "/org/freedesktop/Notifications"

interface :: InterfaceName
interface = "org.freedesktop.Notifications"

busname :: BusName
busname = "org.freedesktop.Notifications"

xmonadNote :: IsString text => Note text
xmonadNote = Note "xmonad" def def "XMonad" "" mempty mempty def

-- * Types

type Notify = Note Text

data Note text = Note
  { notify_app_name       :: !Text                   -- ^ @STRING@
  , notify_replaces_id    :: !NotifyId               -- ^ @UINT32@
  , notify_app_icon       :: !FilePath               -- ^ @STRING@
  , notify_summary        :: !text                   -- ^ @STRING@
  , notify_body           :: !text                   -- ^ @STRING@
  , notify_actions        :: !(M.Map ActionId Text)  -- ^ @ARRAY@ (@[id0, name0, id1, name1, ...]@)
  , notify_hints          :: !(M.Map Text Variant)   -- ^ @DICT@
  , notify_expire_timeout :: !ExpireTimeout          -- ^ @INT32@
  } deriving (Eq, Show)

instance IsString text => Default (Note text) where
  def = urgency 1 $! Note "xmonad" def "" "XMonad" "" def def def

instance IsString text => IsString (Note text) where
  fromString s = body (fromString s) def

newtype NotifyId = NotifyId Word32
  deriving (Eq, Ord, Show, Read, Enum, Num, Real, Bounded, Integral, PrintfArg, Default, IsVariant)

newtype ExpireTimeout = ExpireTimeout Int32
  deriving (Eq, Ord, Show, Read, Enum, Num, Real, Bounded, Integral, PrintfArg, IsVariant)

instance Default ExpireTimeout where
  def = fromIntegral (-1)

type ActionId = Text

data NotificationsClientXS = NotificationsClientXS
  { dbusClient   :: Maybe (MVar.MVar C.Client)
  , lastNotifyId :: NotifyId
  , xmonadIcon   :: Maybe String
  } deriving (Eq)

instance ExtensionClass NotificationsClientXS where
  initialValue = NotificationsClientXS def def def

data CloseReason = Expired
                 | UserDismissed
                 | ByMethodCall
                 | Unspecified
                 deriving (Eq, Show, Read, Enum, Bounded)

instance IsVariant CloseReason where
  toVariant   = toVariant @Word32 . toEnum . fromEnum
  fromVariant = fmap (toEnum . fromEnum @Word32) . fromVariant

data ServerInformation = ServerInformation
  { serverName    :: String
  , serverVendor  :: String
  , serverVersion :: String
  } deriving (Eq, Show, Read)

newtype ImageData = ImageData { fromImageData :: Variant }
  deriving (Eq, Show, IsVariant)

-- ** Markup

data Markup
  = MRaw Text
  | MLit Text
  | MTag Text [(Text,Text)] (Maybe Markup)
  | MAppend Markup Markup
  | MEmpty
  deriving (Eq, Show, Read)

instance Semigroup Markup where
  (<>) = MAppend

instance Monoid Markup where
  mempty = MEmpty

instance IsString Markup where
  fromString s = MRaw (T.pack s)

instance IsVariant Markup where
  toVariant   = toVariant . markupToText
  fromVariant = fmap MRaw . fromVariant

markupToText :: Markup -> TL.Text
markupToText mup = go mup where
  go (MRaw text)             = TL.fromStrict text
  go (MLit text)             = TL.concatMap entities (TL.fromStrict text)
  go (MTag tag attrs minner) = "<" <> TL.fromStrict tag <> toAttrs attrs <> maybe "/>" (\inner -> ">" <> go inner <> "</" <> TL.fromStrict tag <> ">") minner
  go (MAppend ma mb)         = go ma <> go mb
  go (MEmpty)                = mempty

  toAttrs xs = TL.concat [ " " <> TL.fromStrict a <> "=\"" <> TL.concatMap entities (TL.fromStrict v) <> "\"" |(a,v) <- xs]

  entities c = case c of
                 '>'  -> "&gt;"
                 '<'  -> "&lt;"
                 '&'  -> "&amp;"
                 '\"' -> "&quot;"
                 _    -> TL.pack [c]

bold, italic, underline, styleTT :: Markup -> Markup
bold inner      = MTag "b"  [] (Just inner)
italic inner    = MTag "i"  [] (Just inner)
underline inner = MTag "u"  [] (Just inner)
styleTT inner   = MTag "tt" [] (Just inner)

link :: Text -> Markup -> Markup
link url inner = MTag "a" [("href",url)] (Just inner)

-- | @img url description@
img :: Text -> Text -> Markup
img src alt = MTag "img" [("src",src),("alt",alt)] Nothing

-- * Notifications X Client

notify :: IsVariant text => Note text -> X NotifyId
notify nt = do
  xmIcon <- XS.gets xmonadIcon
  withDBusClient $ notify' nt{notify_app_icon=maybe "" id xmIcon}

notify_ :: IsVariant text => Note text -> X ()
notify_ = void . notify

close :: NotifyId -> X Bool
close = withDBusClient . closeNotification

notifyLast :: Notify -> X ()
notifyLast nf =
  XS.gets lastNotifyId >>= \i ->
  notify (replaces i nf) >>= \i' ->
  XS.modify (\s -> s { lastNotifyId = i' })

notifyLastS :: String -> X ()
notifyLastS = notifyLast . fromString

withDBusClient :: (C.Client -> IO a) -> X a
withDBusClient f = XS.gets dbusClient
  >>= X.io . maybe errNoConn MVar.tryReadMVar
  >>= X.io . maybe errNoConn (timeout (1000000 * 2) . f)
  >>= X.io . maybe errTimeout pure
  where
    errNoConn  = E.throw $ C.clientError "No D-Bus client connection established!"
    errTimeout = E.throw $ C.clientError "Timeout"

toVariants :: IsVariant text => Note text -> [Variant]
toVariants Note{..} =
  [ toVariant notify_app_name
  , toVariant notify_replaces_id
  , toVariant notify_app_icon
  , toVariant notify_summary
  , toVariant notify_body
  , toVariant (concat [[k,v]|(k,v)<-M.toAscList notify_actions])
  , toVariant notify_hints
  , toVariant notify_expire_timeout
  ]

-- ** Hooks

-- | Establish Dbus connection.
startupHook :: X ()
startupHook = do
  xdir <- X.getXMonadDir
  icon <-
    let f = (xdir <> "/xmonad.svg")
     in X.io (doesFileExist f) >>= \case
            True -> pure (Just f)
            False -> pure Nothing
  mvar <- X.io MVar.newEmptyMVar
  new  <- XS.modified $ \s ->
    if isNothing (dbusClient s)
       then s { dbusClient = Just mvar
              , xmonadIcon = icon
              }
       else s
  when new $ void . X.io . forkIO $ connect mvar
  where
    connect mvar = do
      res <- E.try $ timeout (1000000 * 5) C.connectSession
      either (X.trace . ("Notify: " <>) . show @E.SomeException) (maybe (pure ()) (MVar.putMVar mvar)) res

-- | Tear down existing dbus connection. Useful when restarting xmonad.
exitHook :: X ()
exitHook = XS.gets dbusClient >>= mapM_ disconnect
  where
    disconnect mvar = liftIO $ do
      X.trace "Disconnecting..."
      timeout (1000000 * 10) $ C.disconnect =<< MVar.takeMVar mvar

urgencyHook :: X.Window -> X ()
urgencyHook = urgencyHookWith $ \tag name ->
  summary (show name) $ body ("requires attention in <b><tt>" ++ tag ++ "</tt></b>") def

urgencyHookWith :: IsVariant text => (X.WorkspaceId -> NW.NamedWindow -> Note text) -> X.Window -> X ()
urgencyHookWith f win = X.withWindowSet $ \wset ->
  X.whenJust (W.findTag win wset) $ \tag -> do
    name <- NW.getName win
    notify_ (f tag name)

-- * org.freedesktop.Notifications

-- ** Methods

notify' :: IsVariant text => Note text -> C.Client -> IO NotifyId
notify' nt c = C.call_ c (notifyMethod "Notify") { methodCallBody = toVariants nt } >>= \r ->
  case fromVariant <$> methodReturnBody r of
    [Just v] -> pure v
    _        -> E.throw (C.clientError $ "unexpected response: " ++ show r)

closeNotification :: NotifyId -> C.Client -> IO Bool
closeNotification nid c =
  C.call_ c (notifyMethod "CloseNotification"){ methodCallBody = [toVariant nid] } >>= \r ->
    case fromVariant <$> methodReturnBody r of
      [Just v] -> return v
      _        -> E.throw (C.clientError $ "unexpected response: " ++ show r)

getCapabilities :: C.Client -> IO [String]
getCapabilities c =
  C.call_ c (notifyMethod "GetCapabilities") >>= \r ->
    case fromVariant `mapM` methodReturnBody r of
      Just caps -> return caps
      _         -> E.throw (C.clientError $ "unexpected response: " ++ show r)

getServerInformation :: C.Client -> IO ServerInformation
getServerInformation c =
  C.call_ c (notifyMethod "GetServerInformation") >>= \r ->
    case fromVariant <$> methodReturnBody r of
      [Just nm, Just vr, Just v] -> return $! ServerInformation nm vr v
      vs                         -> E.throw (C.clientError $ "unexpected response: " ++ show vs)

notifyMethod :: MemberName -> MethodCall
notifyMethod name = (methodCall object interface name) { methodCallDestination = Just busname }

-- ** Signals

onNotifyClosed :: C.Client -> (NotifyId -> CloseReason -> IO ()) -> IO C.SignalHandler
onNotifyClosed client go =
  addHandler client (Just "NotifyClosed") $ \case
    [vid, vr] | Just i <- fromVariant vid, Just r <- fromVariant vr -> go i r
    other -> System.IO.hPutStrLn stderr ("onNotifyClosed: unexpected signal body type: " ++ show other)

onActionInvoked :: C.Client -> (NotifyId -> String -> IO ()) -> IO C.SignalHandler
onActionInvoked client go =
  addHandler client (Just "ActionInvoked") $ \case
    [vid, vr] | Just i <- fromVariant vid, Just r <- fromVariant vr -> go i r
    other -> System.IO.hPutStrLn stderr ("onActionInvoked: unexpected signal body type: " ++ show other)

addHandler :: C.Client -> Maybe MemberName -> ([Variant] -> IO ()) -> IO C.SignalHandler
addHandler client member go = C.addMatch client rule (go . signalBody)
  where rule = C.matchAny { C.matchInterface = Just "org.freedesktop.Notifications", C.matchMember = member }


-- * Notifications

summary :: text -> Note text -> Note text
summary s = \nt -> nt { notify_summary = s }

body :: body -> Note body -> Note body
body b = \nt -> nt { notify_body = b }

-- ** Hints

-- | General hints. @myHint = hintF "hint-name" hintValue@
hintF :: IsVariant hint => Text -> hint -> Note body -> Note body
hintF k v = \nt -> nt { notify_hints = M.alter (\_ -> Just $ toVariant v) k $ notify_hints nt }

appName :: Text -> Notify -> Notify
appName name = \nt -> nt { notify_app_name = name }

-- | The server might display notifications of different categories
-- differently. Notification categories are semi-standardized, see the
-- reference.
--
-- https://developer.gnome.org/notification-spec/#categories
--
-- Hint @category@
category :: String -> Notify -> Notify
category = hintF "category"

replaces :: NotifyId -> Notify -> Notify
replaces nid = \nt -> nt { notify_replaces_id = nid }

-- | This field can be used to specify the calling application's desktop
-- entry, which might be used to for example find out an app icon to display
-- in the notification. (Specify without the @.desktop@ suffix.)
--
-- Hint @desktop-entry@
desktopEntry :: String -> Notify -> Notify
desktopEntry = hintF "desktop-entry"

-- | Expiration timeout (milliseconds)
--
--   * -1: use server settings (default)
--   * 0: never expire
expires :: Integral t => t -> Notify -> Notify
expires t nt = nt { notify_expire_timeout = min (-1) (fromIntegral t) }

-- | Include an action is the notification. If the server supports it, it can
-- display the actions for user to activate. Activation is reported back to
-- the client as signals.
--
-- The default action should have a key (@ActionId@) named @"default"@:
-- @
-- action "default" "Default action"
-- @
action :: ActionId -> Text -> Notify -> Notify
action act name nt = nt { notify_actions = M.insert act name (notify_actions nt) }

-- | The notification should persist until its explicitly dismissed or closed.
--
-- Hint @resident@
resident :: Notify -> Notify
resident = hintF "resident" True

-- | Over-ride server persistence capability, should that exist.
--
-- Hint @transient@
transient :: Notify -> Notify
transient = hintF "transient" True

-- | Set explicit position for the notification.
--
-- Hints @x@ and @y@
position :: Int32 -> Int32 -> Notify -> Notify
position x y = hintF "x" x . hintF "y" y

-- | Urgency Level
--
--   * 0 Low
--   * 1 Normal (default if not set)
--   * 2 Critical
urgency :: Word8 -> Note body -> Note body
urgency = hintF "urgency"

lowUrgency :: Note body -> Note body
lowUrgency = urgency 0

normalUrgency :: Note body -> Note body
normalUrgency = urgency 1

-- | Critical notifications do not usually expire automatically.
critical :: Note body -> Note body
critical = urgency 2

-- ** Icons & images

-- | The server can attempt to use action keys in the notification as icon names.
--
-- Hint @action-icons@
actionIcons :: Notify -> Notify
actionIcons = hintF "action-icons" True

appIcon :: String -> Notify -> Notify
appIcon icon = \nt -> nt { notify_app_icon = icon }

-- | Display image given by path to local file.
--
-- @
-- imagePath "/usr/share/icons/Adwaita/32x32/emotes/face-plain-symbolic.symbolic.png"
-- @
--
-- https://developer.gnome.org/notification-spec/#icons-and-images
--
-- Hint @image-path@.
imagePath :: FilePath -> Notify -> Notify
imagePath path = hintF "image-path" ("file://" ++ path)

-- | Give an image directly in the notification itself. If both this and an
-- icon is specified then behavior is server-specific.
--
-- This is probably only useful if you want to display modified (or generated)
-- images, or images not on the local filesystem. Most notification servers
-- can display images from local files (see "imagePath").
--
-- As defined by the spec, data has to be a raw image (formatted per
-- "imageDataSignature"). To use this hint with other image formats, one
-- could use a library such as https://hackage.haskell.org/package/JuicyPixels
-- or an external tool like ImageMagick to convert any image into the correct
-- format (see "ImageData").
--
-- If the notification server supports it (capability @body-images@), you
-- could opt to use the @<img src="..." alt="..." />@ body markup instead.
imageData :: ImageData -> Notify -> Notify
imageData img = hintF "image-data" img

-- | The image must be given in the data structure of dbus signature @(iiibiiay)@, e.g. RAW.
-- Respectively width, height, rowstride, has alpha, bits per sample, channels, image data.
--
-- @
-- > signatureTypes imageDataSignature
-- [(Int32, Int32, Int32, Bool, Int32, Int32, [Word8])]
-- @
imageDataSignature :: Signature
imageDataSignature = "(iiibiiay)"

-- ** Sound

-- | Play sound given by path to local file.
--
-- Hint @sound-file@
soundFile :: String -> Notify -> Notify
soundFile path = hintF "sound-file" path

-- | Play sound given by themeable sound name. See reference for names.
--
-- http://0pointer.de/public/sound-naming-spec.html
--
-- Hint @sound-name@
soundName :: String -> Notify -> Notify
soundName name = hintF "sound-name" name

-- | Hint @suppress-sound@
suppressSound :: Notify -> Notify
suppressSound = hintF "suppress-sound" True

