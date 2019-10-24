{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
------------------------------------------------------------------------------
-- |
-- Module      : MyNotify
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
-- https://hackage.haskell.org/package/fdo-notify-0.3.1/docs/DBus-Notify.html#t:Notification
-- https://developer.gnome.org/pango/stable/PangoMarkupFormat.html
--
------------------------------------------------------------------------------

module MyNotify (
   -- * Hooks
   startupHook,
   exitHook,
   urgencyHook,
   -- * Notify
   Notify,
   NotifyId,
   notifyLast,
   notifyLastS,
   notify_,
   notify,
   summary,
   replaces,
   body,
   -- ** Urgency
   urgency0,
   urgency1,
   urgency2,
   ) where

import           Prelude

import           Control.Concurrent          (forkIO)
import qualified Control.Concurrent.MVar     as MVar
import qualified Control.Exception           as E
import           Control.Monad

import           Data.Int                    (Int32)
import qualified Data.Map                    as M
import           Data.Maybe                  (isNothing)
import           Data.String                 (IsString(..))
import           Data.Word                   (Word32, Word8)
import           Data.Text (Text)
import qualified Data.Text as T

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

import           Util                        (logError, logInfo)

-- * Notifications X Client

notify_ :: Notify -> X ()
notify_ = void . notify

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

-- | Install signal handlers for notification events (action invoked, notification closed).
-- Catch 'C.ClientError' if you need to determine whether handlers were installed or not.
handleNotifySignalsX :: Maybe (NotifyId -> String -> IO ()) -> Maybe (NotifyId -> CloseReason -> IO ()) -> X ()
handleNotifySignalsX ah ch = withDBusClient $ liftM2 (>>) (forM_ ah . onActionInvoked) (forM_ ch . onNotifyClosed)

-- ** Hooks

-- | Establish Dbus connection.
startupHook :: X ()
startupHook = do
  mvar <- X.io MVar.newEmptyMVar
  new  <- XS.modified $ \s ->
    if isNothing (dbusClient s) then s { dbusClient = Just mvar } else s
  when new $ void . X.io . forkIO $ connect mvar
  where
    connect mvar = do
      res <- E.try $ timeout (1000000 * 5) C.connectSession
      either (logError . ("Notify: " <>) . show @E.SomeException) (maybe (pure ()) (MVar.putMVar mvar)) res

-- | Tear down existing dbus connection. Useful when restarting xmonad.
exitHook :: X ()
exitHook = XS.gets dbusClient >>= mapM_ disconnect
  where
    disconnect mvar = liftIO $ do
      logInfo "Disconnecting..."
      timeout (1000000 * 10) $ C.disconnect =<< MVar.takeMVar mvar

urgencyHook :: X.Window -> X ()
urgencyHook w = do
  ws <- X.gets X.windowset
  X.whenJust (W.findTag w ws) $ \tag -> do
    wname <- NW.getName w
    notify_ $
      summary (show wname) $
        body ("requires attention in <b><tt>" ++ tag ++ "</tt></b>") def

-- ** Client State

-- | Note that @const . const ()@ or similar for any handler here is /not/ equivalent to @Nothing@.
-- A @Just@ handler will attempt to install the D-Bus handler. @Nothing@ won't install the handler at all.
data NotificationsClientXS = NotificationsClientXS
  { dbusClient                :: Maybe (MVar.MVar C.Client)
  , actionInvokedHandler      :: Maybe (NotifyId -> String -> IO ())
  , notificationClosedHandler :: Maybe (NotifyId -> CloseReason -> IO ())
  , lastNotifyId              :: NotifyId
  }

instance ExtensionClass NotificationsClientXS where
  initialValue = NotificationsClientXS Nothing Nothing Nothing 0

instance Eq NotificationsClientXS where
  a == b = (==) (dbusClient a) (dbusClient b)

-- * org.freedesktop.Notifications

object :: ObjectPath
object = "/org/freedesktop/Notifications"

interface :: InterfaceName
interface = "org.freedesktop.Notifications"

busname :: BusName
busname = "org.freedesktop.Notifications"

-- ** Methods

notify :: Notify -> X NotifyId
notify nt = withDBusClient $ \c ->
  C.call_ c (notifyMethod "Notify") { methodCallBody = toVariants nt } >>= \r ->
    case fromVariant <$> methodReturnBody r of
      [Just v] -> pure v
      _        -> E.throw (C.clientError $ "unexpected response: " ++ show r)

close :: NotifyId -> X Bool
close nid = withDBusClient $ \c ->
  C.call_ c (notifyMethod "CloseNotification"){ methodCallBody = [toVariant nid] } >>= \r ->
    case fromVariant <$> methodReturnBody r of
      [Just v] -> return v
      _        -> E.throw (C.clientError $ "unexpected response: " ++ show r)

data CloseReason = Expired | UserDismissed | ByMethodCall | Unspecified
  deriving (Eq, Enum, Bounded, Show, Read)

instance IsVariant CloseReason where
  toVariant   = toVariant @Word32 . toEnum . fromEnum
  fromVariant = fmap (toEnum . fromEnum @Word32) . fromVariant

getCapabilities :: X [String]
getCapabilities = withDBusClient $ \c ->
  C.call_ c (notifyMethod "GetCapabilities") >>= \r ->
    case fromVariant `mapM` methodReturnBody r of
      Just caps -> return caps
      _         -> E.throw (C.clientError $ "unexpected response: " ++ show r)

getServerInformation :: X ServerInformation
getServerInformation = withDBusClient $ \c ->
  C.call_ c (notifyMethod "GetServerInformation") >>= \r ->
    case fromVariant <$> methodReturnBody r of
      [Just nm, Just vr, Just v] -> return $! ServerInformation nm vr v
      _                          -> E.throw (C.clientError $ "unexpected response: " ++ show r)

data ServerInformation = ServerInformation { serverName, serverVendor, serverVersion :: !String }
  deriving (Eq, Show, Read)

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

newtype NotifyId = NotifyId { unNotifyId :: Word32 }
  deriving (Eq, Num, Show, Read, IsVariant)

-- | ARRAY - @[id0, name0, id1, name1, ...]@
type Actions = [String]

data Notify = Notify
  { notify_app_name       :: Text
  , notify_replaces_id    :: !NotifyId -- UINT32
  , notify_app_icon       :: Variant  -- STRING
  , notify_summary        :: Text
  , notify_body           :: Variant  -- STRING
  , notify_actions        :: !Actions
  , notify_hints          :: !(M.Map String Variant) -- DICT
  , notify_expire_timeout :: !Int32  -- INT32
  } deriving (Eq, Show)

toVariants :: Notify -> [Variant]
toVariants Notify{..} =
  [ toVariant notify_app_name
  , toVariant notify_replaces_id
  , notify_app_icon
  , toVariant notify_summary
  , notify_body
  , toVariant notify_actions
  , toVariant notify_hints
  , toVariant notify_expire_timeout
  ]

instance Default Notify where
  def = urgency0 $ Notify "xmonad" 0 (sv "") "XMonad" (sv "") [] mempty (-1)
    where sv = toVariant @String

instance IsString Notify where
  fromString str
    | (s,_:b) <- span (/='\n') str = body ("<tt>" ++ b ++ "</tt>") $ summary s def
    | otherwise = body str def

-- ** Summary & body text

class    IsText a      where toText :: a -> Text
instance IsText Text   where toText = id
instance IsText String where toText = T.pack

summary :: IsText text => text -> Notify -> Notify
summary s = \nt -> nt { notify_summary = toText s }

body :: IsText text => text -> Notify -> Notify
body b = \nt -> nt { notify_body = toVariant $ toText b }

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

-- ** Hints

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
category c = hintF "category" (c::String)

replaces :: NotifyId -> Notify -> Notify
replaces nid = \nt -> nt { notify_replaces_id = nid }

-- | This field can be used to specify the calling application's desktop
-- entry, which might be used to for example find out an app icon to display
-- in the notification. (Specify without the @.desktop@ suffix.)
--
-- Hint @desktop-entry@
desktopEntry :: String -> Notify -> Notify
desktopEntry e = hintF "desktop-entry" (e::String)

expireTimeout :: Integral t => t -> Notify -> Notify
expireTimeout t = \nt -> nt { notify_expire_timeout = fromIntegral t }

-- | Include an action is the notification. If the server supports it, it can
-- display the actions for user to activate. Activation is reported back to
-- the client as signals.
action :: String -> String -> Notify -> Notify
action k v n = n { notify_actions = k : v : notify_actions n }

-- | General hints. @myHint = hintF "hint-name" hintValue@
hintF :: IsVariant v => String -> v -> Notify -> Notify
hintF k v = \nt -> nt { notify_hints = M.alter (\_ -> Just $ toVariant v) k $ notify_hints nt }

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
setXY :: Int32 -> Int32 -> Notify -> Notify
setXY x y = hintF "x" x . hintF "y" y

-- ** Urgency

-- | From low to high.
urgency0, urgency1, urgency2 :: Notify -> Notify
urgency0 = hintF "urgency" (0::Word8)
urgency1 = hintF "urgency" (1::Word8)
urgency2 = hintF "urgency" (2::Word8)

-- ** Icons & images

-- | The server can attempt to use action keys in the notification as icon names.
--
-- Hint @action-icons@
actionIcons :: Notify -> Notify
actionIcons = hintF "action-icons" True

appIcon :: String -> Notify -> Notify
appIcon icon = \nt -> nt { notify_app_icon = toVariant icon }

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

newtype ImageData = ImageData { fromImageData :: Variant }
  deriving (Eq, Show, IsVariant)

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

