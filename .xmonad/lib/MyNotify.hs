{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module         : MyNotify
-- Maintainer     : Samuli Thomasson <samuli.thomasson@relexsolutions.com>
-- Stability      : experimental
-- Portability    : non-portable
------------------------------------------------------------------------------
module MyNotify (module MyNotify) where

-- https://hackage.haskell.org/package/fdo-notify-0.3.1/docs/DBus-Notify.html#t:Notification

-- https://developer.gnome.org/pango/stable/PangoMarkupFormat.html

import           Prelude
import qualified Control.Exception           as E
import           Control.Monad
import           Data.Int                    (Int32)
import           Data.Word                   (Word32, Word8)
import           Data.Kind (Type)
import           Data.String (IsString(..))
import           Data.Proxy
import           System.IO (hPrint, stderr)

import qualified Data.Map                    as M
import           DBus hiding (Type)
import qualified DBus.Client                 as C

import           XMonad                      (Default (..), MonadIO (liftIO), X, ExtensionClass(..))
import qualified XMonad.Util.ExtensibleState as XS

-- * XMonad

-- | Establish Dbus connection.
startupHook :: X ()
startupHook = startupHook' C.connectSession

-- | Like notifyStartupHook but lets you decide how the DBus is procured.
-- In most cases though one should just use notifyStartupHook.
startupHook' :: IO C.Client -> X ()
-- XXX: will this block / for how long if the bus happens to be unresponsive?
startupHook' mkClient = liftIO (E.try mkClient) >>= \case
    Right client -> XS.modify $ \st -> st { dbusClient = Just client }
    Left (e::E.SomeException) -> liftIO $ hPrint stderr (show e)

-- | Propagates 'C.ClientError' from dbus in case the method call failed.
callNotificationMethodX :: NotificationsMessage msg => msg -> X (Maybe (ReturnBody msg))
callNotificationMethodX m = withDBusClient (liftIO . flip callNotificationMethod m)

-- | Throws an 'C.ClientError' if dbus connection has not been established.
callNotificationMethodX' :: NotificationsMessage msg => msg -> X (ReturnBody msg)
callNotificationMethodX' m = callNotificationMethodX m >>= maybe (E.throw e) return
  where e = C.clientError "No D-Bus client connection established!"

-- | Install signal handlers for notification events.
-- Catch 'C.ClientError' if you need to determine whether handlers were
-- installed or not.
handleNotifySignalsX
    :: Maybe (NotifyId -> String -> IO ()) -- ^ Action invoked
    -> Maybe (NotifyId -> CloseReason -> IO ()) -- ^ Notification closed
    -> X (Maybe ()) -- ^ Nothing means nothing done, no client
handleNotifySignalsX ai nc = withDBusClient $ \client -> liftIO $ do
    mapM_ (onNotifyClosed client) nc
    mapM_ (onActionInvoked client) ai

withDBusClient :: (C.Client -> X a) -> X (Maybe a)
withDBusClient f = XS.gets dbusClient >>= mapM f

-- |
-- Note that @const . const ()@ or similar for any handler here is /not/
-- functionally equivalent to @Nothing@: a @Just@ will cause (an attempt to)
-- install the D-Bus handler, while @Nothing@ won't install the handler at
-- all.
data NotificationsClientXS = NotificationsClientXS
    { dbusClient :: Maybe C.Client
    , actionInvokedHandler :: Maybe (NotifyId -> String -> IO ())
    , notificationClosedHandler :: Maybe (NotifyId -> CloseReason -> IO ())
    }

instance ExtensionClass NotificationsClientXS where
    initialValue = NotificationsClientXS Nothing Nothing Nothing

-- ** Methods

notify_ :: Notify -> X ()
notify_ nt = void (callNotificationMethodX' nt)

notify :: Notify -> X NotifyId
notify nt = callNotificationMethodX' nt

close :: NotifyId -> X Bool
close i = callNotificationMethodX' (CloseNotification i)

getCapabilities :: X [String]
getCapabilities = callNotificationMethodX' GetCapabilities

getServerInformation :: X ServerInformation
getServerInformation = callNotificationMethodX' GetServerInformation

-- * Notifications

withBody :: String -> String -> Notify
withBody s b = summary s $ txtBody b def

appName, appIcon, summary, txtBody :: String -> Notify -> Notify
appName name = \nt -> nt { notify_app_name = toVariant name }
appIcon icon = \nt -> nt { notify_app_icon = toVariant icon }
summary text = \nt -> nt { notify_summary = toVariant text }
txtBody text = \nt -> nt { notify_body = toVariant text }

replaces :: NotifyId -> Notify -> Notify
replaces nid = \nt -> nt { notify_replaces_id = toVariant nid }

expireTimeout :: Integral t => t -> Notify -> Notify
expireTimeout t = \nt -> nt { notify_expire_timeout = toVariant @Int32 (fromIntegral t) }

-- | Include an action is the notification. If the server supports it, it can
-- display the actions for user to activate. Activation is reported back to
-- the client as signals.
action :: String -> String -> Notify -> Notify
action k v n = n { notify_actions = k : v : notify_actions n }

-- ** Body markup
--
-- $bodyMarkup
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

-- | From low to high.
urgency0, urgency1, urgency2 :: Notify -> Notify
urgency0 = hintF "urgency" (0::Word8)
urgency1 = hintF "urgency" (1::Word8)
urgency2 = hintF "urgency" (2::Word8)

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
suppressSound  = hintF "suppress-sound" True

-- | The server might display notifications of different categories
-- differently. Notification categories are semi-standardized, see the
-- reference.
--
-- https://developer.gnome.org/notification-spec/#categories
--
-- Hint @category@
category :: String -> Notify -> Notify
category c = hintF "category" (c::String)

-- | This field can be used to specify the calling application's desktop
-- entry, which might be used to for example find out an app icon to display
-- in the notification. (Specify without the @.desktop@ suffix.)
--
-- Hint @desktop-entry@
desktopEntry :: String -> Notify -> Notify
desktopEntry e = hintF "desktop-entry" (e::String)

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

-- | The server can attempt to use action keys in the notification as icon
-- names.
--
-- Hint @action-icons@
actionIcons :: Notify -> Notify
actionIcons = hintF "action-icons" True

-- | Set explicit position for the notification.
--
-- Hints @x@ and @y@
setXY :: Int32 -> Int32 -> Notify -> Notify
setXY x y = hintF "x" x . hintF "y" y

-- | Set new custom hints:
-- @
-- myHint = hintF "hint-name" hintValue
-- @
hintF :: IsVariant v => String -> v -> Notify -> Notify
hintF k v = \nt -> nt { notify_hints = M.alter (\_ -> Just $ toVariant v) k $ notify_hints nt }

-- ** Images

newtype ImageData = ImageData Variant deriving (Eq, Show, IsVariant)

-- | The image must be given in the data structure of dbus signature
-- (iiibiiay) which describes the width, height, rowstride, has alpha, bits
-- per sample, channels and image data respectively.
--
-- @
-- > signatureTypes imageDataSignature
-- [(Int32, Int32, Int32, Bool, Int32, Int32, [Word8])]
-- @
imageDataSignature :: Signature
imageDataSignature = "(iiibiiay)"

-- * Notfifications D-Bus interface

-- ** Messages

notificationMethod :: NotificationsMessage msg => msg -> MethodCall
notificationMethod msg =
  (methodCall "/org/freedesktop/Notifications" "org.freedesktop.Notifications" (methodName msg))
    { methodCallDestination = Just "org.freedesktop.Notifications"
    , methodCallBody = methodArgs msg
    }

callNotificationMethod :: forall msg. NotificationsMessage msg => C.Client -> msg -> IO (ReturnBody msg)
callNotificationMethod cl msg = do
    body <- methodReturnBody <$> C.call_ cl (notificationMethod msg)
    case returnBody @msg Proxy body of
        Nothing -> error ("Notifications: unexpected response type: " ++ show body)
        Just ret -> return ret

class NotificationsMessage msg where

    type ReturnBody msg :: Type

    methodName :: msg -> MemberName
    methodArgs :: msg -> [Variant]

    -- | Parse a method return body.
    returnBody :: Proxy msg -> [Variant] -> Maybe (ReturnBody msg)

    -- | In case of method error, if "returnError" yields a Just value then
    -- that return value is used and method error suppressed.
    --
    -- The default implementation always returns Nothing.
    returnError :: Proxy msg -> ErrorName -> [Variant] -> Maybe (ReturnBody msg)
    returnError _ _ _ = Nothing

data Notify = Notify
    { notify_app_name :: Variant -- STRING
    , notify_replaces_id :: Variant -- UINT32
    , notify_app_icon :: Variant -- STRING
    , notify_summary :: Variant -- STRING
    , notify_body :: Variant -- STRING
    , notify_actions :: [String] -- ARRAY - @[id0, name0, id1, name1, ...]@
    , notify_hints :: M.Map String Variant -- DICT
    , notify_expire_timeout :: Variant -- INT32
    }

newtype NotifyId = NotifyId { unNotifyId :: Word32 } deriving (Eq, Num, IsVariant)

instance Default Notify where
  def = Notify es (toVariant (0::Word32)) es es es [] mempty (toVariant @Int32 (-1))
    where es = toVariant (""::String)

instance IsString Notify where
    fromString str = case span (/='\n') str of
        (_, []) -> summary str def
        (s,_:b) -> txtBody b $ summary s def

-- | UINT32 org.freedesktop.Notifications.Notify
instance NotificationsMessage Notify where
  type ReturnBody Notify = NotifyId
  methodName _ = "Notify"
  methodArgs Notify{..} =
    [ notify_app_name, notify_replaces_id, notify_app_icon
    , notify_summary, notify_body, toVariant notify_actions
    , toVariant notify_hints, notify_expire_timeout ]
  returnBody _ [v] = NotifyId <$> fromVariant v
  returnBody _ _ = Nothing

-- | Notification exists: forcefully closes a notification (no method reply).
data CloseNotification = CloseNotification { closeNotificationId :: NotifyId }

-- | If notification no longer exists, an empty error is received.
instance NotificationsMessage CloseNotification where
  type ReturnBody CloseNotification = Bool
  methodName _ = "CloseNotification"
  methodArgs cn = [toVariant $ unNotifyId $ closeNotificationId cn]
  returnBody _ _ = Nothing

data GetCapabilities = GetCapabilities

instance NotificationsMessage GetCapabilities where
  type ReturnBody GetCapabilities = [String]
  methodName _ = "GetCapabilities"
  methodArgs _ = []
  returnBody _ vs = mapM fromVariant vs

data GetServerInformation = GetServerInformation

data ServerInformation = ServerInformation { serverName, serverVendor, serverVersion :: String }

instance NotificationsMessage GetServerInformation where
  type ReturnBody GetServerInformation = ServerInformation
  methodName _ = "GetServerInformation"
  methodArgs _ = []
  returnBody _ vs | [Just nm, Just vr, Just v] <- map fromVariant vs = Just $ ServerInformation nm vr v
                | otherwise = Nothing

-- ** Signals

onNotifyClosed :: C.Client -> (NotifyId -> CloseReason -> IO ()) -> IO C.SignalHandler
onNotifyClosed client go = addHandler client (Just "NotifyClosed") $ \case
    [vid, vr] | Just i <- fromVariant vid, Just r <- fromVariant vr -> go i r
    other -> error ("onNotifyClosed: unexpected signal body type: " ++ show other)

onActionInvoked :: C.Client -> (NotifyId -> String -> IO ()) -> IO C.SignalHandler
onActionInvoked client go = addHandler client (Just "ActionInvoked") $ \case
    [vid, vr] | Just i <- fromVariant vid, Just r <- fromVariant vr -> go i r
    other -> error ("onActionInvoked: unexpected signal body type: " ++ show other)

addHandler :: C.Client -> Maybe MemberName -> ([Variant] -> IO ()) -> IO C.SignalHandler
addHandler client member go = C.addMatch client rule (go . signalBody)
  where
    rule = C.matchAny { C.matchInterface = Just "org.freedesktop.Notifications", C.matchMember = member }

data CloseReason = Expired
                 | UserDismissed
                 | ByMethodCall
                 | Unspecified
                 deriving (Eq, Show, Enum)

instance IsVariant CloseReason where
    toVariant = toVariant @Word32 . toEnum . fromEnum
    fromVariant = fmap (toEnum . fromEnum @Word32) . fromVariant
