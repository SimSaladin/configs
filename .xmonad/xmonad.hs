{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RebindableSyntax          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# OPTIONS_GHC
    -Weverything
    -Wno-partial-type-signatures
    -Wno-missing-import-lists
    -Wno-unused-top-binds
    -Wno-unused-do-bind #-}
------------------------------------------------------------------------------
-- |
-- Module         : Main
-- Description    : XMonad configuration
-- Copyright      : (C) 2011-2019 Samuli Thomasson
-- License        : BSD-3
-- Maintainer     : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability      : experimental
-- Portability    : non-portable
-- Creation Date  : Jun 15 2011 [22:30:53]
--
------------------------------------------------------------------------------
module Main (main) where

import           Prelude                               ((>>))
import           Control.Monad                         (guard, when, forM_, forM, join, filterM, liftM2, unless, (>=>))
import qualified Control.Exception                     as Exception (try, bracket)
import qualified Data.List                             as Data.List
import qualified Data.Map                              as Data.Map
import           Data.Maybe
import           Data.Monoid                           (All (All))
import           Data.Ratio                            ((%))
import Codec.Binary.UTF8.String (encodeString)
-------------------------------------------------------------------------
import           XMonad.Config.Prime                   hiding ((>>))
import qualified XMonad.Config.Prime                   as Arr ((>>))
import qualified XMonad                                as X
import qualified XMonad.StackSet                       as W
-------------------------------------------------------------------------
import qualified XMonad.Actions.CopyWindow             as CopyWindow
import qualified XMonad.Actions.CycleRecentWS          as A.CycleRecentWS
import qualified XMonad.Actions.CycleWS                as A.CycleWS
import qualified XMonad.Actions.DynamicWorkspaceGroups as DynWSG
import qualified XMonad.Actions.DynamicWorkspaceOrder  as DynWSO
import qualified XMonad.Actions.DynamicWorkspaces      as DynWS
import qualified XMonad.Actions.FlexibleManipulate     as A.FlexMan
import qualified XMonad.Actions.FloatSnap              as A.FloatSnap (snapMove, snapGrow, snapShrink)
import qualified XMonad.Actions.FloatKeys              as A.FloatKeys (keysMoveWindowTo)
import qualified XMonad.Actions.NoBorders              as NoBorders
import qualified XMonad.Actions.PhysicalScreens        as A.PScreens
import qualified XMonad.Actions.RotSlaves              as RotSlaves
import qualified XMonad.Actions.UpdatePointer          as A (updatePointer) -- or: X.A.Warp
--import qualified XMonad.Actions.UpdateFocus            as A (focusOnMouseMove, adjustEventInput)
import qualified XMonad.Actions.Minimize               as A.Minimize
import qualified XMonad.Actions.GridSelect             as GS
-------------------------------------------------------------------------
import qualified XMonad.Hooks.DebugStack               as H.Debug
import qualified XMonad.Hooks.DebugEvents              as H.Debug (debugEventsHook)
-- import qualified XMonad.Hooks.ManageDebug              as H.Debug
import qualified XMonad.Hooks.DynamicBars              as H.DynBars
import           XMonad.Hooks.DynamicLog               as H.PP
import qualified XMonad.Hooks.EwmhDesktops             as H.EWMH
import qualified XMonad.Hooks.FadeWindows              as H.FadeWindows (isFloating)
import qualified XMonad.Hooks.FloatNext                as H.FloatNext
import qualified XMonad.Hooks.InsertPosition           as H.Insert
import           XMonad.Hooks.ManageHelpers            as H.ManageHelpers
import qualified XMonad.Hooks.ManageDocks              as H.ManageDocks
import qualified XMonad.Hooks.Place                    as H.Place
import qualified XMonad.Hooks.PositionStoreHooks       as H.PSF
import qualified XMonad.Hooks.UrgencyHook              as H.Urgency
import qualified XMonad.Hooks.Minimize                 as H.Minimize
-------------------------------------------------------------------------
import qualified XMonad.Layout.BoringWindows           as L.BoringWindows
import qualified XMonad.Layout.LayoutHints             as L.Hints (hintsEventHook, layoutHintsToCenter)
import qualified XMonad.Layout.NoBorders               as L.NoBorders
import qualified XMonad.Layout.Magnifier               as L.Magnifier (MagnifyMsg(..), magnifierOff)
import qualified XMonad.Layout.Maximize                as L.Maximize (maximizeWithPadding, maximizeRestore)
import qualified XMonad.Layout.Minimize                as L.Minimize (minimize)
import qualified XMonad.Layout.Renamed                 as L.Renamed
import qualified XMonad.Layout.Spacing                 as L.Spacing (Border(..), spacingRaw, toggleScreenSpacingEnabled, toggleWindowSpacingEnabled)
import qualified XMonad.Layout.MultiToggle             as L.Toggle
import qualified XMonad.Layout.WindowNavigation        as L.WinNav (configurableNavigation, navigateColor, Navigate(..))
--import qualified XMonad.Layout.PerWorkspace            as L (onWorkspace)
--import qualified XMonad.Layout.SubLayouts              as L.Sub ()
import qualified XMonad.Layout.ComboP                  as L (combineTwoP, CombineTwoP)
import qualified XMonad.Layout.IfMax                   as L (ifMax, IfMax) -- TODO
import qualified XMonad.Layout.BinarySpacePartition    as L.BSP (emptyBSP, TreeBalance(..), ResizeDirectional(..))
import qualified XMonad.Layout.PositionStoreFloat      as L.PSF (positionStoreFloat)
import           XMonad.Layout.ThreeColumns               (ThreeCol (ThreeColMid))
import           XMonad.Layout.OneBig                     (OneBig(OneBig))
import           XMonad.Layout.Simplest                   (Simplest(Simplest))
import           XMonad.Layout.TwoPane                    (TwoPane(TwoPane))
import           XMonad.Layout.GridVariants               (Grid(Grid))
import           XMonad.Layout.MultiToggle.Instances      (StdTransformers(NOBORDERS, MIRROR, NBFULL))
import           XMonad.Layout.Reflect                    (REFLECTX(..), REFLECTY(..))
import           XMonad.Layout.LayoutModifier          as L (ModifiedLayout(..))
-------------------------------------------------------------------------
import qualified XMonad.Prompt                         as XP
import qualified XMonad.Prompt.AppLauncher             as XP (launchApp)
import qualified XMonad.Prompt.Directory               as XP (directoryPrompt)
import qualified XMonad.Prompt.FuzzyMatch              as XP (fuzzyMatch) -- , fuzzySort)
import qualified XMonad.Prompt.Input                   as XP ((?+), inputPromptWithCompl)
import qualified XMonad.Prompt.Man                     as XP (manPrompt)
import qualified XMonad.Prompt.Pass                    as XP (passPrompt)
import qualified XMonad.Prompt.Shell                   as XP (getCommands, getShellCompl)
import qualified XMonad.Prompt.Window                  as XP (allWindows, WindowPrompt(Goto))
import qualified XMonad.Prompt.Window                  as XP (windowPrompt)
-------------------------------------------------------------------------
import qualified XMonad.Util.EZConfig                  as EZConfig (mkNamedKeymap)
import           XMonad.Util.Font                       (Align(..))
import qualified XMonad.Util.ExtensibleState           as XS
import           XMonad.Util.Loggers                   as Loggers
import           XMonad.Util.NamedActions              (NamedAction, addName, oneName)
import qualified XMonad.Util.NamedActions              as NA
import qualified XMonad.Util.NamedWindows              as NW
import qualified XMonad.Util.Run                       as Run
import           XMonad.Util.Types                     (Direction1D (Prev,Next),
                                                        Direction2D (D,U,L,R))
import           XMonad.Util.WindowProperties          (Property(..), getProp32)
--import           XMonad.Util.WindowState
import           XMonad.Util.XUtils                    (fi)
import           XMonad.Util.Ungrab                    (unGrab)
-------------------------------------------------------------------------
import qualified System.Directory                      as Directory
import qualified System.Exit                           as System.Exit (exitSuccess)
import qualified System.IO                             as System.IO
import           System.Process                        (showCommandForUser)
-------------------------------------------------------------------------
import qualified Foreign.C.Types                       as Foreign.C (CLong)
import qualified Graphics.X11.Xinerama                 (getScreenInfo)

import qualified MyNotify as Notify

-- NOTE:
--
-- withWindowSet   f === gets windowset >>= f
-- modifyWindowSet f === modify $ windowset %~ f
-- windows f         === modifyWindowSet f >> refresh
-- W.allWindows

-- * Themes and decorations

-- dark
colorBorder = colCyan
colorFg0 = colBase01
colorFg  = colBase1
colorFg1 = colCyan
colorFg2 = colMagenta
colorFg3 = colYellow
colorBg  = colBase03
colorBg2 = colBase02
colorUrgent1 = colGreen
-- light
-- fgColor     = colBase02
-- bgColor     = colBase3
-- fgHLight    = colMagenta
-- bgHLight    = colBase2
-- borderColor = colBase1

colBase03, colBase02, colBase01, colBase00, colBase0, colBase1, colBase2, colBase3, colYellow, colOrange, colRed, colMagenta, colViolet, colBlue, colCyan, colGreen :: String
colBase03  = "#002b36"
colBase02  = "#073642"
colBase01  = "#586e75"
colBase00  = "#657b83"
colBase0   = "#839496"
colBase1   = "#93a1a1"
colBase2   = "#eee8d5"
colBase3   = "#fdf6e3"
colYellow  = "#b58900"
colOrange  = "#cb4b16"
colRed     = "#dc322f"
colMagenta = "#d33682"
colViolet  = "#6c71c4"
colBlue    = "#268bd2"
colCyan    = "#2aa198"
colGreen   = "#859900"

gsconfig1 :: GS.HasColorizer a => GS.GSConfig a
gsconfig1 = def
  { GS.gs_cellwidth   = 360
  , GS.gs_cellheight  = 24
  , GS.gs_cellpadding = 5
  , GS.gs_navigate    = GS.navNSearch
  , GS.gs_font        = font 18
  , GS.gs_bordercolor = colorBorder
  }

xpconfig :: XP.XPConfig
xpconfig = def
  { XP.completionKey     = (0, xK_Tab)
  , XP.changeModeKey     = xK_grave -- ` i.e. <S-#>
  , XP.promptKeymap      = XP.defaultXPKeymap
  , XP.position          = XP.CenteredAt (1%3) (1%2)
  , XP.height            = 35 -- per row
  , XP.maxComplRows      = Just 40
  , XP.historySize       = 512
  , XP.historyFilter     = XP.deleteAllDuplicates
  , XP.searchPredicate   = XP.fuzzyMatch
  , XP.font              = bfont 22
  , XP.promptBorderWidth = 2
  , XP.borderColor = colorBorder
  , XP.fgColor = colorFg, XP.bgColor = colorBg
  , XP.fgHLight = colorFg2, XP.bgHLight = colorBg2
  }
xpconfigAuto = xpconfig { XP.autoComplete = Just 500000 }
xpconfigNoHist = xpconfig { XP.historySize = 0 }

-- 22 myFont, 32 myFontLarge
font :: Int -> String
font size
  | size <= 14 = "xft:xos4 Terminus:pixelsize=" ++ show size
  | otherwise  = "xft:TerminessTTF Nerd Font:pixelsize=" ++ show size

bfont :: Int -> String
bfont size = font size ++ ":style=bold"

statusBarsLogHook :: X ()
statusBarsLogHook = H.DynBars.multiPPFormat fmt focusPP focusPP{ ppCurrent = xmobarColor colBlue "" }
  where
    fmt pp = maybe "" encodeString <$>
        mconcat [ xmobarColorL colorFg3 colorBg2
                  . wrapL "[" "]"
                  . xmobarColorL colorFg colorBg2
                  . fixedWidthL AlignCenter "-" 30 $ Loggers.logLayout
                , logSp 1
                , logWS pp
                , logSp 1
                , xmobarColorL colorFg3 colorBg2
                  . wrapL "[" "]"
                  . xmobarColorL colorFg colorBg2
                  . fixedWidthL AlignCenter "-" 50 $ onLogger xmobarStrip Loggers.logCurrent
                ]
    focusPP = def
            { ppUrgent          = xmobarColor colorUrgent1 ""
            , ppVisible         = xmobarColor colorFg1 ""
            , ppCurrent         = xmobarColor colorFg2 ""
            , ppHidden          = xmobarColor colorFg  ""
            , ppHiddenNoWindows = xmobarColor colorFg0 ""
            }

logWS :: PP -> Logger
logWS pp = do
    sort'   <- DynWSO.getSortByOrder
    urgents <- H.Urgency.readUrgents
    copies  <- CopyWindow.wsContainingCopies
    wset    <- gets windowset
    hwins   <- filterM (runQuery isHidden) (W.allWindows wset)
    sepByConcat (pure $ pure $ ppWsSep pp)
        [ ppKeyL (pure $ pure k) <> ppWS wset copies urgents hwins ws (pure $ pure $ W.tag ws)
            | (ws,k) <- sort' (W.workspaces wset) `zip` myWsKeys']
  where
    ppWS :: WindowSet -> [WorkspaceId] -> [Window] -> [Window] -> WindowSpace -> Logger -> Logger
    ppWS wset copies urgents hwins w
      | isJust $ W.stack w >>= W.filter (`elem` urgents)          = onLogger (ppUrgent pp)
      | W.tag w == W.tag (W.workspace (W.current wset))           = onLogger (ppCurrent pp)
      | W.tag w `elem` map (W.tag . W.workspace) (W.visible wset) = onLogger (ppVisible pp)
      | W.tag w `elem` copies                                     = onLogger (ppCopies)
      | isJust $ W.stack w >>= W.filter (`notElem` hwins)         = onLogger (ppHidden pp) . fixedWidthL AlignCenter "." 8
      | otherwise                                                 = onLogger (ppHiddenNoWindows pp . const "…")
    ppKeyL    = xmobarColorL colorFg3 "" . onLogger (++":")
    ppCopies  = xmobarColor colYellow ""

sepByConcat :: (Monoid a, Foldable t) => a -> t a -> a
sepByConcat a = mconcat . Data.List.intersperse a . foldMap pure

-- * Main, config, hooks

main :: IO ()
main = xmonad' $ do
    {- some settings -}
    terminal           =: "urxvtc"
    focusFollowsMouse  =: True
    clickJustFocuses   =: False
    {- window borders -}
    borderWidth        =: 1
    focusedBorderColor =: colorBorder
    normalBorderColor  =: colBase02
    let navBorderColor    = colBase00
        urgentBorderColor = colGreen
    {- bindings -}
    modMask       =: mod4Mask
    mouseBindings =+ myMouseBindings
    myKeys
    {- event maks -}
    clientMask =+ structureNotifyMask .|. enterWindowMask .|. propertyChangeMask .|. focusChangeMask
    rootMask =+ substructureRedirectMask .|. substructureNotifyMask .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask .|. buttonPressMask
    {- workspaces -}
    setWorkspaces
    {- layout and modifiers -}
    resetLayout switchableLayouts
    modifyLayout (L.Toggle.mkToggle1 MIRROR) -- mirror + reflect x + y is most intuitive when mirror goes first
    modifyLayout (L.Toggle.mkToggle1 REFLECTX)
    modifyLayout (L.Toggle.mkToggle1 REFLECTY)
    modifyLayout (L.Toggle.mkToggle1 NOBORDERS)
    modifyLayout (L.Toggle.mkToggle1 MY_HINT)
    modifyLayout L.Magnifier.magnifierOff -- name "Magnifier (off)"
    modifyLayout (L.Toggle.mkToggle1 NBFULL)
    modifyLayout L.BoringWindows.boringWindows
    modifyLayout L.Minimize.minimize -- name "Minimize"
    modifyLayout (L.NoBorders.lessBorders MyNoBorders)
    modifyLayout (L.WinNav.configurableNavigation $ L.WinNav.navigateColor navBorderColor)
    modifyLayout (L.Maximize.maximizeWithPadding 80) -- NOTE: windowNavigation before this so it works properly
    modifyLayout (mySpacing 15 5)           -- NOTE: Interacts with windowNavigation
    modifyLayout H.ManageDocks.avoidStruts  -- NOTE: apply late to avoid structs more often
    {- Notifications via DBus -}
    startupHook     =+ Notify.startupHook
    {- Debug -}
    startupHook     =+ debugStartupHook
    handleEventHook =+ debugEventHook
    {- Docks -}
    apply H.ManageDocks.docks -- manageDocks / docksStartupHook / docksEventHook
    {- Layout hacks -}
    manageHook      =+ myWindowPlacements
    handleEventHook =+ H.Minimize.minimizeEventHook -- Handle minimize/maximize requests
    handleEventHook =+ myFullscreenEventHook        -- A "_NET_WM_STATE_FULLSCREEN" hack for fullscreening mpv which might be unnecessary now.
    handleEventHook =+ L.Hints.hintsEventHook       -- Refreshes the layout whenever a window changes its hints.
    {- Statusbars -}
    startupHook     =+ H.DynBars.dynStatusBarStartup myStatusBar (return ())
    handleEventHook =+ H.DynBars.dynStatusBarEventHook myStatusBar (return ())
    logHook         =+ statusBarsLogHook
    {- Urgency hooks -}
    addUrgencyHook myUrgencyConfig (H.Urgency.BorderUrgencyHook urgentBorderColor)
    addUrgencyHook myUrgencyConfig notificationUrgencyHook
    {- EWMH -}
    startupHook     =+ H.EWMH.ewmhDesktopsStartup   -- Initialize and advertise EWMH support
    handleEventHook =+ H.EWMH.ewmhDesktopsEventHook -- Intercepts _NET_CURRENT_DESKTOP, _NET_WM_DESKTOP, _NET_ACTIVE_WINDOW
    logHook         =+ H.EWMH.ewmhDesktopsLogHook
    {- X.U.PositionStore -}
    manageHook      =+ H.PSF.positionStoreManageHook Nothing -- X.U.PositionStore
    handleEventHook =+ H.PSF.positionStoreEventHook
    {- UpdatePointer -}
    logHook         =+ whenX notspam (A.updatePointer (0.5, 0.5) (0.4, 0.4))
  where
    (>>) :: Arr x y -> Arr y z -> Arr x z
    (>>) = (Arr.>>)

    myUrgencyConfig = H.Urgency.urgencyConfig { H.Urgency.suppressWhen = H.Urgency.Focused }

    myWindowPlacements = composeOne
      $ scratchpadsManageHook myScratchpads ++
      [ className =? "Xmessage"      -?> doCenterFloat
      , className =? "Display"       -?> doCenterFloat
      , appName   =? "info-terminal" -?> doRectFloat $ W.RationalRect 0.2 0.1 0.6 0.6
      , className =? "Xmag"          -?> doCenterFloat
      , (Data.List.isInfixOf "mpv-float" <$> stringProperty "WM_CLASS") -?> doCenterFloat
      , transience -- if transient then position to parent
      , isDialog  -?> doCenterFloat
      , willFloat -?> myFloat
      , Just <$> H.Insert.insertPosition H.Insert.Below H.Insert.Newer -- Default: Above Newer
      ]

    myFloat   = H.Place.placeHook (H.Place.withGaps (30, 30, 30, 30) $ H.Place.smart (0.5,0.5)) <+> H.FloatNext.floatNextHook
    willFloat = (not <$> H.FadeWindows.isFloating) <&&> liftX (H.FloatNext.willFloatNext <||> H.FloatNext.willFloatAllNew)

    notspam :: X Bool
    notspam = asks currentEvent >>= \case
        Just PropertyEvent{..} -> return False -- don't fire on spammy property updates (status bars)
        _                      -> return True

switchableLayouts :: _ Window
switchableLayouts =   L.BSP.emptyBSP
                  ||| Tall 1 (1/30) (2/3)
                  ||| OneBig (2/3) (2/3)
                  ||| ThreeColMid 1 (1/30) (1/3)
                  ||| withToolbar (Grid (16/9))
                  ||| L.PSF.positionStoreFloat

-- | Map input ScreenId to non-overlapping unique screen physically sorted
-- screens.
myStatusBar :: H.DynBars.DynamicStatusBar
myStatusBar sID@(S i) = mapUniqueScreensPhysical >>= \tab -> case Data.List.lookup sID tab of
    Nothing -> System.IO.openFile "/dev/null" System.IO.WriteMode
    Just _  -> shellPipe $ "xmobar -o -x " <> show i

-- Generic

xmonad' :: (Default a, LayoutClass l Window, Read (l Window)) => (a -> IO (XConfig l)) -> IO ()
xmonad' f = f def >>= launch

addUrgencyHook :: (LayoutClass l Window, H.Urgency.UrgencyHook hook) => H.Urgency.UrgencyConfig -> hook -> Prime l l
addUrgencyHook config hook = apply $ H.Urgency.withUrgencyHookC hook config

notificationUrgencyHook :: Window -> X ()
notificationUrgencyHook w = gets windowset >>= \ws -> whenJust (W.findTag w ws) (\t -> hook' t . show =<< NW.getName w)
  where hook' tag name = let text = "requires attention in <b><tt>" ++ tag ++ "</tt></b>"
                          in Notify.notify_ (Notify.withBody name text)

-- Layout

myMax :: LayoutClass l Window => Dimension -> l Window -> _
myMax padding = L.Maximize.maximizeWithPadding padding

mySpacing :: LayoutClass l Window => Integer -> Integer -> l Window -> _
mySpacing sd wd = let f n = L.Spacing.Border n n n n
                      in L.Spacing.spacingRaw False (f sd) False (f wd) False

withToolbar :: LayoutClass l Window => l Window -> L.CombineTwoP (TwoPane ()) Full l Window
withToolbar inner = L.combineTwoP (TwoPane (1/32) (1/8)) Full inner (Const False) -- TODO

-- Saved workspaces

setWorkspaces :: Prime l l
setWorkspaces = applyIO (\xc -> getWorkspaces >>= \ws -> return xc { X.workspaces = ws })
  where
    getWorkspaces = wsFile >>= Directory.doesFileExist >>= \exists -> if exists
            then wsFile >>= fmap read . readFile
            else return ["1","2","3"]

wsFile :: IO FilePath
wsFile = fmap (++ "/.xmonad/workspaces") Directory.getHomeDirectory

saveWorkspaces :: X ()
saveWorkspaces = do
    to <- liftIO wsFile
    dynSort <- DynWSO.getSortByOrder
    liftIO . writeFile to . show =<< gets (map W.tag . dynSort . W.workspaces . windowset)

-- Debug

-- |
-- use Display in IO:
--    Exception.bracket (openDisplay "") closeDisplay \d -> …
--
-- eventTable
-- AnyEvent{}
-- ConfigureRequestEvent{}
-- ConfigureEvent{}
-- MapRequestEvent{}
-- KeyEvent{}
-- ButtonEvent{}
-- MotionEvent{..}               -- needs additional eventmask
-- DestroyWindowEvent{}
-- UnmapEvent{}
-- MapNotifyEvent{}
-- MappingNotifyEvent{}
-- CrossingEvent{}               -- pointer crosses from window to another
-- SelectionRequest{}
-- SelectionClear{}
-- PropertyEvent{..}
-- ExposeEvent{}
-- ClientMessageEvent{}
-- RRScreenChangeNotifyEvent{}
-- RRNotifyEvent{}
-- RRCrtcChangeNotifyEvent{}
-- RROutputChangeNotifyEvent{}
-- RROutputPropertyNotifyEvent{}
-- ScreenSaverNotifyEvent{}

debugStartupHook :: X ()
debugStartupHook = mapM getAtom aNames >>= \as -> XS.modify (\s -> s{debugIgnoreProps = flip elem as})
  where
    aNames = ["WM_STATE", "WM_NAME", "WM_ICON_NAME", "WM_LOCALE_NAME", "WM_CLIENT_MACHINE", "WM_NORMAL_HINTS"
             , "_NET_WM_STATE", "_NET_WM_NAME", "_NET_WM_ICON_NAME", "_NET_WM_DESKTOP"]

debugEventHook :: Event -> X All
debugEventHook e =
  XS.gets debugIgnoreProps >>= \p ->
  case e of
      KeyEvent{} -> return (All True)
      ConfigureEvent{} -> return (All True)
      DestroyWindowEvent{} -> return (All True)
      PropertyEvent{..} | p ev_atom -> return (All True)
      -- _ -> H.Debug.debugEventsHook e
      _ -> return (All True)

-- * MyXS

data MyXS = MyXS
    { lastNotifyId :: Notify.NotifyId
    , notifyBase   :: Notify.Notify
    , debugIgnoreProps :: !(Atom -> Bool)
    }

instance ExtensionClass MyXS where
    initialValue = MyXS 0 (Notify.summary "XMonad" $ Notify.urgency0 def) (const True)

-- * Bindings

-- | Mouse button actions. Kensington button keycodes:
--  top-right: button8
--  top-left: button2
--  bottom-right: button3
--  bottom-left: button1
--  scroll wheel: button4, button5
myMouseBindings :: [((KeyMask, Button), Window -> X ())]
myMouseBindings =
  [ ((modm,               button1), \w -> A.FlexMan.mouseWindow A.FlexMan.discrete w)
  , ((modm .|. shiftMask, button1), \w -> A.FlexMan.mouseWindow A.FlexMan.resize w)
  , ((modm,               button2), \w -> windows $ W.focusWindow w >> W.swapMaster)
  , ((controlMask,        button3), \_ -> shell "mpv --really-quiet --profile=preview $(xclip -o) &>/dev/null &")
  , ((shiftMask,          8), \_ -> shell "mpv --really-quiet --profile=preview $(xclip -o) &>/dev/null &")
  ]
  where
    modm = mod4Mask

myKeys :: Prime l l
myKeys = apply $ keysFromKeymaps showKeysNotify $
  -- common
  [ ("M-<Return>",  oneName $ inTerm def "" [])
  , ("M-S-c",       addName "Kill focused window (instance)" CopyWindow.kill1)
  , ("M-m",         addName "(Un)maximize focused" $ withFocused maximizeRestoreTiled)
  , ("M-+",         volInc)
  , ("M--",         volDec)
  , ("M-$",         lockScreen)
  , ("M-q",         restartXMonad)
  , ("M-S-q",       exitXMonad)
  , ("M-<Esc>",     addName "Debug: focused WindowSet (notify)" $ H.Debug.debugStackString >>= notifyS)
  -- scratchpads
  , ("M-#",         oneName $ toggleScratchpad myScratchpads "temp")
  , ("M-S-<Return>",addName "Minimize all scratchpads" $ minimizeScratchpads myScratchpads)
  -- layout
  , ("M-<Space>",   NA.sendMessage' NextLayout)
  , ("M-S-<Space>", addName "Reset layout" $ setLayout . X.layoutHook =<< asks config)
  , ("M-x",         NA.sendMessage' Shrink)
  , ("M-S-x",       NA.sendMessage' Expand)
  , ("M-.",         NA.sendMessage' (IncMasterN 1))
  , ("M-,",         NA.sendMessage' (IncMasterN (-1)))
  -- workspaces
  , ("M-y",         addName "Focus recently used WS's (cycle)"  $ cycleRecentHiddenWS [xK_Super_L, xK_Alt_L] xK_y xK_p)
  , ("M-S-n",       addName "Shift current WS (forward)"        $ DynWSO.swapWith Next A.CycleWS.AnyWS)
  , ("M-S-p",       addName "Shift current WS (backward)"       $ DynWSO.swapWith Prev A.CycleWS.AnyWS)
  , ("M-C-<Right>", addName "Focus screen (prev)"               A.CycleWS.prevScreen)
  , ("M-C-<Left>" , addName "Focus screen (next)"               A.CycleWS.nextScreen)

  -- M-b
  , ("M-b S-f", addName "Toggle: Full (override layout)"        $ sendMessage $ L.Toggle.Toggle NBFULL)
  , ("M-b b",   addName "Toggle: All borders (modifier)"        $ sendMessage $ L.Toggle.Toggle NOBORDERS)
  , ("M-b g",   addName "Toggle: avoid structs (modifier)"      $ sendMessage   H.ManageDocks.ToggleStruts)
  , ("M-b h",   addName "Toggle: hinting (modifier)"            $ sendMessage $ L.Toggle.Toggle MY_HINT)
  , ("M-b l",   addName "Toggle: Magnify (modifier)"            $ sendMessage   L.Magnifier.Toggle)
  , ("M-b m",   addName "Toggle: Mirror (modifier)"             $ sendMessage $ L.Toggle.Toggle MIRROR)
  , ("M-b x",   addName "Toggle: ReflectX (modifier)"           $ sendMessage $ L.Toggle.Toggle REFLECTX)
  , ("M-b y",   addName "Toggle: ReflectY (modifier)"           $ sendMessage $ L.Toggle.Toggle REFLECTY)
  , ("M-b M-b", addName "Toggle: Borders (focused window)"      $ withFocused NoBorders.toggleBorder >> refresh)
  , ("M-b f",   addName "Toggle: Float new windows"               H.FloatNext.toggleFloatAllNew)
  , ("M-b s",   addName "Toggle: spacing"                       $ L.Spacing.toggleScreenSpacingEnabled >> L.Spacing.toggleWindowSpacingEnabled)
  -- M-c
  , ("M-c c",   oneName $ toggleScratchpad myScratchpads "ncmpcpp")
  , ("M-c m",   oneName $ toggleScratchpad myScratchpads "pulsemixer")
  , ("M-c t",   mpc ["toggle"])
  , ("M-c n",   mpc ["next"])
  , ("M-c p",   mpc ["prev"])
  -- , ("M-c c",   mpc ["crop"])
  , ("M-c y",   mpc ["single"])
  , ("M-c r",   mpc ["random"])
  -- M-f
  , ("M-f f",   windowGotoPrompt)
   , ("M-f s",   addName   "Sink"              $ withFocused $ windows . W.sink)
   , ("M-f m",   addName   "Swap Master" $ windows W.swapMaster) -- TODO needs boring
   , ("M-f n",   addName   "Swap Up"     $ windows W.swapUp)
   , ("M-f p",   addName   "Swap Down"   $ windows W.swapDown)
   , ("M-f M-m", addName   "Focus Master"        L.BoringWindows.focusMaster)
   , ("M-f M-n", addName   "Focus Up"            L.BoringWindows.focusUp)
   , ("M-f M-p", addName   "Focus Down"          L.BoringWindows.focusDown)
   , ("M-f u",   addName   "Focus Urgent"        H.Urgency.focusUrgent)
   , ("M-f M-.", addName   "Rotate Up"       RotSlaves.rotAllUp)
   , ("M-f M-,", addName   "Rotate Down"     RotSlaves.rotAllDown)
   , ("M-f .",   addName   "Rotate Slaves Up"    RotSlaves.rotSlavesUp)
   , ("M-f ,",   addName   "Rotate Slaves Down"  RotSlaves.rotSlavesDown)
   , ("M-f c",   addName   "Center On Screen" $ withFocused centerOnScreen)
  -- M-g
  , ("M-g g",   addName "Focus WS by name (XP)"       $ DynWS.selectWorkspace xpconfig)
  , ("M-g s",   addName "Focus WS by window (GS)"     $ GS.goToSelected gsconfig1)
  , ("M-g t",   addName "Focus WS by name (GS)"       $ GS.gridselectWorkspace gsconfig1 W.greedyView)
  , ("M-g n",   addName "Add new WS (XP)"             $ DynWS.addWorkspacePrompt xpconfig >> saveWorkspaces)
  , ("M-g d",   addName "Remove current WS if empty"  $ removeNoVisibleWorkspace saveWorkspaces)
  , ("M-g r",   addName "Rename current WS (XP)"      $ DynWS.renameWorkspace xpconfig >> saveWorkspaces)
  , ("M-g c",   addName "Copy window to current WS by name (XP)" $ DynWS.withWorkspace xpconfigAuto (windows . CopyWindow.copy))
  , ("M-g m",   addName "Move window to current WS by name (XP)" $ DynWS.withWorkspace xpconfigAuto (windows . W.shift))
  , ("M-g M-g", addName "Focus WSGroup (XP)"          $ DynWSG.promptWSGroupView xpconfigAuto "Focus WSGroup: ")
  , ("M-g M-n", addName "Name current WSGroup (XP)"   $ DynWSG.promptWSGroupAdd xpconfig "Name WSGroup: ")
  , ("M-g M-d", addName "Forget current WSGroup (XP)" $ DynWSG.promptWSGroupForget xpconfig "Forget WSGroup: ")
  -- M-r
  , ("M-r b",   oneName $ toggleScratchpad myScratchpads "bluetoothctl")
  , ("M-r j",   oneName $ toggleScratchpad myScratchpads "journal")
  , ("M-r c",   addName "clipmenu" $ run' "clipmenu" [])
  , ("M-r d",   rangerPrompt)
  , ("M-r e",   evincePrompt)
  , ("M-r g",   gimpPrompt)
  , ("M-r h",   manPrompt)
  , ("M-r m",   addName "xmag (magnify selected area)" $ unGrab >> Run.safeSpawn "xmag" ["-mag", "2", "-source", "960x540"])
  , ("M-r p",   passPrompt)
  , ("M-r q",   qutebrowserPrompt)
  , ("M-r r",   spawnAsServicePrompt)
  , ("M-r M-r", spawnInTerminalPrompt)
  , ("M-r s",   scratchpadPrompt)
  , ("M-r t",   addName "tmux/tmuxp (load or create session)" tmuxPrompt)
  , ("M-r u",   chromeAppPrompt)
  , ("M-r v",   addName "play (clipboard) with mpv (LQ)" $ shell "mpv --profile=preview $(xclip -o) &>/dev/null &")
  ]
  ++ keysWith "Focus screen"              "M-"      mySKeys screenIds (A.PScreens.viewScreen def)
  ++ keysWith "Move focused to screen"    "M-S-"    mySKeys screenIds (A.PScreens.sendToScreen def)
  ++ keysWith "Swap & focus screen"       "M-C-"    mySKeys screenIds (\s -> greedyViewScreen s >> A.PScreens.viewScreen def s)
  ++ keysWith "Swap & view screen"        "M-M1-"   mySKeys screenIds greedyViewScreen
  --
  ++ keysWith "View workspace"            "M-; "    myWsKeys' [0..] (withNthWorkspace W.greedyView)
  ++ keysWith "Copy focused to workspace" "M-; M-"  myWsKeys' [0..] (withNthWorkspace CopyWindow.copy)
  ++ keysWith "Move focused to workspace" "M-S-; "  myWsKeys' [0..] (withNthWorkspace W.shift)
  --
  ++ keysWith "Focus window" "M-"      directionKeys directions (sendMessage . L.WinNav.Go)
  ++ keysWith "Swap window"  "M-S-"    directionKeys directions (sendMessage . L.WinNav.Swap)
  -- M-f
  ++ keysWith "Move float"   "M-f "    directionKeys directions (withFocused . flip A.FloatSnap.snapMove   Nothing)
  ++ keysWith "Grow float"   "M-f S-"  directionKeys directions (withFocused . flip A.FloatSnap.snapGrow   Nothing)
  ++ keysWith "Shrink float" "M-f C-"  directionKeys directions (withFocused . flip A.FloatSnap.snapShrink Nothing)
  ++
  -- myMediaKeys
  [ ("<XF86AudioPlay>",         mpc ["toggle"])
  , ("<XF86AudioStop>",         mpc ["stop"])
  , ("<XF86AudioPrev>",         mpc ["prev"])
  , ("<XF86AudioNext>",         mpc ["next"])
  , ("<XF86AudioLowerVolume>",  volDec)
  , ("<XF86AudioRaiseVolume>",  volInc)
  , ("<XF86MonBrightnessUp>",   backlightInc)
  , ("<XF86MonBrightnessDown>", backlightDec)
  --, ("<XF86AudioMute>",
  ]

keysFromKeymaps :: ([String] -> X ()) -> [(String, NamedAction)] -> XConfig l -> XConfig l
keysFromKeymaps flash keymap = \xc -> xc { X.keys = mkTop }
  where
    mkTop :: XConfig Layout -> Data.Map.Map (KeyMask, KeySym) (X ())
    mkTop conf = Data.Map.fromList $ map (\(k,a) -> (k, NA.getAction a)) $ mkAll conf "" keymap
    mkAll :: XConfig Layout -> String -> [(String, NamedAction)] -> [((KeyMask, KeySym), NamedAction)]
    mkAll conf k ks =
      let (bare, s) = Data.List.partition (notElem ' ' . fst) ks
          subs      = map (mkSub conf) $ groupSub s
          km        = EZConfig.mkNamedKeymap conf $ help ++ bare ++ subs
          km'       = EZConfig.mkNamedKeymap conf $ help ++ bare ++ [(sk,NA.addName "(submap)" $ return ())|(sk,_)<-subs]
          help      = [("M-<F1>", flash' "Help" km') | null k]
          in km
    mkSub :: XConfig Layout -> (String, [(String, NamedAction)]) -> (String, NamedAction)
    mkSub conf (k, ks) =
      let km   = mkAll conf k ks
          help = flash' ("submap " ++ k) km
          in (k, NA.submapDefaultName help km)
    groupSub :: [(String, NamedAction)] -> [(String, [(String, NamedAction)])]
    groupSub = Data.Map.toList . Data.Map.fromListWith (++) . mapMaybe f
      where f :: (String, NamedAction) -> Maybe (String, [(String, NamedAction)])
            f (k,a) = case span (/=' ') k of
                            (k1,' ':k2) -> Just (k1, [(k2, a)])
                            ( _,     _) -> Nothing
    flash' desc = addName desc . flash . NA.showKm . (NA.subtitle desc :)

showKeysTerm :: [String] -> X ()
showKeysTerm lns = fst $ inTerm dialog "/bin/bash" ["-c", showCommandForUser "echo" [unlines lns] ++ " | less -"]

showKeysNotify :: [String] -> X ()
showKeysNotify [] = return ()
showKeysNotify (top:lns) = Notify.notify_ $ Notify.urgency0 $ Notify.withBody top ("<tt>" ++ unlines lns ++ "</tt>")

myWsKeys', myWsKeys :: [String]
myWsKeys' = map pure ['a'..'z']
myWsKeys  = map ("; "++) myWsKeys'
mySKeys, directionKeys :: [String]
mySKeys       = ["w","v","z"]
directionKeys = ["j","k","h","l"]
--
screenIds     = [A.PScreens.P 0 ..]
directions    = [D,U,L,R]

-- | @keysWith "common description" modifier keys values action@
keysWith :: Show v => String -> String -> [String] -> [v] -> (v -> X ()) -> [(String, NamedAction)]
keysWith nm m ks vs f = zipWith (\k v -> (m ++ k, addName (nm ++ " ("++ show v ++")") $ f v)) ks vs

-- * MyTransformers (MultiToggle)

-- | Toggling hints
data MyTransformers = MY_HINT deriving (Read, Show, Eq)

instance L.Toggle.Transformer MyTransformers Window where
  transform MY_HINT x k = k (L.Renamed.renamed [L.Renamed.PrependWords "Hint"] $ L.Hints.layoutHintsToCenter x) (\(L.ModifiedLayout _ (L.ModifiedLayout _ l)) -> l)

-- * NoBorders

-- | Passed to "lessBorders". Remove borders on a tiled window
-- covering the whole screen *on per screen basis*.
data MyNoBorders = MyNoBorders deriving (Read, Show)

instance L.NoBorders.SetsAmbiguous MyNoBorders where
  hiddens MyNoBorders wset _lr _mst wrs = tiled ++ floating where
    tiled = case wrs of [(w,_)] -> [w]
                        _       -> []
    floating = do
      s  <- W.screens wset -- [Screen i l a sid sd]
      sr <- [screenRect $ W.screenDetail s] -- Rectangle
      w  <- W.integrate' $ W.stack $ W.workspace s -- [Window]
      wr <- maybeToList $ Data.Map.lookup w (W.floating wset) -- [RationalRect]
      guard (sr == scaleRationalRect sr wr)
      [w]

-- * Scratchpads

myScratchpads :: [Scratchpad]
myScratchpads = mkXScratchpads
  [ ("temp",          tmux def{termName=Just"pad-temp"} "temp",     appName =? "pad-temp")
  , ("ncmpcpp",       term "pad-ncmpcpp"      "ncmpcpp" [],         appName =? "pad-ncmpcpp")
  , ("pulsemixer",    term "pad-pulsemixer"   "pulsemixer" [],      appName =? "pad-pulsemixer")
  , ("journal",       term "pad-journal"      "journalctl" ["-f"],  appName =? "pad-journal")
  , ("sdcv",          term "pad-sdcv"         "sdcv" [],            appName =? "pad-sdcv")
  , ("bluetoothctl",  term "pad-bluetoothctl" "bluetoothctl" [],    appName =? "pad-bluetoothctl")
  ] (doRectFloat (W.RationalRect 0.2 0.1 0.6 0.6))
  where
    term nm c a = fst $ inTerm def{termName = Just nm} c a

-- | Custom implementation: affected by NamedScratchpads and ExclusiveScratchpads.
data Scratchpad = SP
  { spName      :: String
  , spCmd       :: X ()
  , spQuery     :: Query Bool
  , spHook      :: ManageHook
  , spExclusive :: [String]
  }

mkXScratchpads :: [(String, X (), Query Bool)] -> ManageHook -> [Scratchpad]
mkXScratchpads xs h = res where
    res = [SP n c q h' ns | (n,c,q) <- xs, let ns = Data.List.delete n $ map spName res]
    h'  = h <> H.Insert.insertPosition H.Insert.Above H.Insert.Newer

scratchpadsManageHook :: [Scratchpad] -> [MaybeManageHook]
scratchpadsManageHook xs = [spQuery sp -?> spHook sp | sp <- xs]

toggleScratchpad :: [Scratchpad] -> String -> (X (), String)
toggleScratchpad spads name = (toggleScratchpad' spads name, name ++ " (scratchpad)")

toggleScratchpad' :: [Scratchpad] -> String -> X ()
toggleScratchpad' spads name =
    -- first, minimize any pads exclusive with target.
    -- then, look for an existing instance in focused workspace.
    -- if that fails, look for an instance across all windows.
    whenJust (Data.List.find ((name ==) . spName) spads)
    $ \sp -> minimizeScratchpads [x | x <- spads, spName x `elem` spExclusive sp]
    >> withWindowSet (filterM (runQuery (spQuery sp)) . currentWindows) >>= \case
        w : _ -> toggleWindow Nothing w
        []    -> withWindowSet (filterM (runQuery (spQuery sp)) . W.allWindows) >>= \case
            w : _ -> toggleWindow Nothing w
            []    -> spCmd sp

minimizeScratchpads :: [Scratchpad] -> X ()
minimizeScratchpads spads = withWindowSet $ mapM_ (\w -> whenX (runQuery (padsQ spads) w) (toggleWindow (Just False) w)) . currentWindows

padsQ :: [Scratchpad] -> Query Bool
padsQ (spad : spads) = foldr (<||>) (spQuery spad) [spQuery sp | sp <- spads]
padsQ [] = return False

-- * Actions

lockScreen, restartXMonad, exitXMonad :: NamedAction
restartXMonad = addName "Restart XMonad" $ saveWorkspaces >> restart "xmonad-my" True
exitXMonad    = addName "Exit XMonad" $ io System.Exit.exitSuccess
lockScreen    = oneName $ run "physlock" []

backlightInc  = oneName $ run "xbacklight" ["-inc","5"] -- brightness up
backlightDec  = oneName $ run "xbacklight" ["-dec","10"] -- brightness down

-- * Notify

myNotify :: (Notify.Notify -> Notify.Notify) -> X ()
myNotify = \f -> XS.get >>= \MyXS{lastNotifyId, notifyBase} ->
    Notify.notify (f $ Notify.replaces lastNotifyId notifyBase) >>= \i' -> XS.modify (\s -> s{lastNotifyId = i'})

notifyS :: String -> X ()
notifyS str = myNotify (Notify.txtBody ("<tt>" ++ str ++ "</tt>"))

-- * Applications

mpc :: [String] -> NamedAction
mpc cmd = oneName $ run "mpc" cmd

-- |
volInc = oneName $ run "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "+3%"]
volDec = oneName $ run "pactl" ["set-sink-volume", "@DEFAULT_SINK@", "-3%"]

tmuxp :: Terminal -> String -> X ()
tmuxp term session = fst $ inTerm term { termOpts = termOpts term ++ ["-sl","0"] } "tmuxp" ["load", "-y", session]

tmux :: Terminal -> String -> X ()
tmux term session = fst $ inTerm term { termOpts = termOpts term ++ ["-sl","0"] } "tmux" ["new-session", "-A", "-s", session]

passPrompt, evincePrompt, gimpPrompt, manPrompt, rangerPrompt :: NamedAction
-- XXX extend with support for editing and other actions too
passPrompt            = addName "pass"   $ XP.passPrompt xpconfig
evincePrompt          = addName "evince" $ XP.launchApp xpconfig "evince"
gimpPrompt            = addName "gimp"   $ XP.launchApp xpconfig "gimp"
manPrompt             = addName "man"    $ XP.manPrompt xpconfig
rangerPrompt          = addName "ranger" $ XP.directoryPrompt xpconfig "" $ fst . inTerm dialog "ranger" . pure
windowGotoPrompt      = addName "Goto window" $ XP.windowPrompt xpconfigAuto XP.Goto XP.allWindows
chromeAppPrompt       = inputPromptWithHistCompl "chrome --app" (fst . chromeApp)
spawnAsServicePrompt  = executablePrompt "Run cmd via systemd-run" $ \xs -> fst $ service Nothing Nothing (head $ words xs) (tail $ words xs)
spawnInTerminalPrompt = executablePrompt "Run cmd in terminal" (\cmd -> fst $ inTerm def { termHold = True } "/bin/bash" ["-c", cmd])

-- | Note: google-chrome as single process is really single process (without
-- sandboxing). So if starting with that unit name fails, we just launch chrome
-- directly and it'll open a new window in existing session.
--
-- Some wrapper and systemd-socket solution could perhaps be better.
--
-- Also, chrome likes to stay in the background even after you close
-- all its windows. For that, uncheck the "Continue running background apps when
-- Chrome is closed" flag in chrome://settings.
chromeApp :: String -> (X (), String)
chromeApp url = service (Just "google-chrome") (Just url) "google-chrome-stable" ["--app=" ++ url]

-- | --qt-arg name app_name etc. https://peter.sh/experiments/chromium-command-line-switches/
-- $XDG_RUNTIME_DIR/qutebrowser/$session/runtime/ipc-*
-- /usr/share/qutebrowser/scripts/open_url_in_instance.sh
qutebrowser :: String -> (X (), String)
qutebrowser p = service (Just "qutebrowser") (Just p) "qutebrowser" ["-r", p]

qutebrowserPrompt :: NamedAction
qutebrowserPrompt = addName name $ do
  compl <- liftIO getCompl
  XP.inputPromptWithCompl xpconfigNoHist name compl XP.?+ \p -> unless (null p) $ fst $ qutebrowser p
  where
    name = "qutebrowser"
    getCompl = Directory.getXdgDirectory Directory.XdgData "qutebrowser"
      >>= Exception.try . Directory.listDirectory
      >>= either (\e -> onIOError e >> pure []) (pure . f)
      >>= pure . XP.mkComplFunFromList'

    f = Data.List.filter $ \x -> not
      $ ("-qutebrowser" `Data.List.isSuffixOf` x)
      || ("." `Data.List.isPrefixOf` x)
      || (x `elem` ["null", "userscripts", "qtwebengine_dictionaries", "blocked-hosts"])

scratchpadPrompt :: NamedAction
scratchpadPrompt = addName name $ do
  XP.inputPromptWithCompl xpconfig name compl XP.?+ fst . toggleScratchpad myScratchpads
  where
    name  = "scratchpad"
    compl = XP.mkComplFunFromList' (map spName myScratchpads)

data TmuxPrompt = TmuxPrompt -- { tpAvailable :: [String], tpActive :: [String], tpComps :: [String] }

instance XP.XPrompt TmuxPrompt where
  showXPrompt TmuxPrompt = "TMUX: "

  -- The compl to print on command line, when tab is pressed. Not used in multi-mode.
  -- default: getNextOfLastWord
  nextCompletion _ x xs = XP.getNextCompletion x (map (takeWhile (/='\x1F')) xs)

  -- The string to pass to ComplFunction.
  -- default: getLastWord
  commandToComplete _ = filter (/='\x1F')
  -- Generate string to compare with the command presently in the command line.
  -- Also the string to form on the command-line from a completion.
  -- default: id
  completionToCommand _ = takeWhile (/='\x1F')

  --completionFunction
  --modeAction

tmuxPrompt :: X ()
tmuxPrompt = do
    comp <- liftIO tmuxpComplFun
    XP.mkXPromptWithReturn TmuxPrompt xpconfig (pure . comp) return XP.?+
        (\is -> (if null (comp is) then tmux def else tmuxp def) is)

-- tmux display-message -p '#{S:#{?session_attached,#{session_name} ,}}'
tmuxpComplFun :: IO (String -> [String])
tmuxpComplFun = getKnown >>= \known -> getAttached >>= \as ->
    let l = as ++ [k | k <- known, k `notElem` map (takeWhile (/= sep)) as]
        in return $ \case
              [] -> l
              s  -> filter (XP.fuzzyMatch s . filter (/='\x1F')) l
  where

    getKnown = Directory.getXdgDirectory Directory.XdgConfig "tmuxp"
      >>= Exception.try . Directory.listDirectory
      >>= either (\e -> onIOError e >> pure []) (pure . f)
    f xs = [takeWhile (/= '.') x | x <- xs, ".yaml" `Data.List.isSuffixOf` x]

    getAttached :: IO [String]
    getAttached = lines <$> runWithInput "tmux" ["list-sessions", "-F", "#S" ++ [sep] ++ " [#{W:#{E:window-status-format} ,#{E:window-status-current-format} }] #{?session_attached,(attached),}" ] ""
    sep = '\x1F'

dialog :: Terminal
dialog = def
  { termName = Just "info-terminal"
  , termOpts = ["-geometry", "130x40"]
  }

-- * Util

-- ** Process

inputPromptWithHistCompl :: String -> (String -> X ()) -> NamedAction
inputPromptWithHistCompl name cb = addName name $
    XP.inputPromptWithCompl xpconfig name (XP.historyCompletionP (== name ++ ": ")) XP.?+ cb

executablePrompt :: String -> (String -> X ()) -> NamedAction
executablePrompt name cb = addName name $ do
    comp <- flip XP.getShellCompl (XP.searchPredicate xpconfig) <$> liftIO XP.getCommands
    XP.inputPromptWithCompl xpconfig name comp XP.?+ cb

run :: FilePath -> [String] -> (X (), String)
run prog args = (run' prog args, showCommandForUser prog args)

run' :: FilePath -> [String] -> X ()
run' prog args = logInfo ("run: " ++ showCommandForUser prog args) >> Run.safeSpawn prog args

shell :: String -> X ()
shell cmd = logInfo ("shell: " ++ cmd) >> Run.unsafeSpawn cmd

-- | @service name prog args@
service :: Maybe String -> Maybe String -> FilePath -> [String] -> (X (), String)
service name i prog args = (, show name ++ show i) $ do
    logInfo ("service: " ++ show name ++ "@" ++ show i ++ ": " ++ showCommandForUser prog args)
    i' <- forM i systemdEscape
    let munit = ["--unit=" ++ nm ++ maybe "" ("@"++) i' | nm <- maybeToList name]
    Run.safeSpawn "systemd-run" $ ["--user","--collect"] ++ munit ++ (prog : args)

runWithInput :: MonadIO m => FilePath -> [String] -> String -> m String
runWithInput prog args input = do
    logInfo ("runWithInput: " ++ showCommandForUser prog args)
    Run.runProcessWithInput prog args input

shellPipe :: MonadIO m => String -> m System.IO.Handle
shellPipe cmd = logInfo ("shellPipe: " ++ cmd) >> Run.spawnPipe cmd

-- ** Terminal

data Terminal = Terminal
  { termName :: Maybe String -- ^ set name
  , termOpts :: [String] -- ^ arguments to pass to terminal
  , termCWD  :: Maybe String -- ^ set -cd
  , termHold :: Bool -- ^ don't automatically destroy window
  } deriving (Eq, Show)

instance Default Terminal where
    def = Terminal Nothing [] Nothing False

inTerm :: Terminal -> FilePath -> [String] -> (X (), String)
inTerm Terminal{..} prog args = (, "Term: " ++ showCommandForUser prog args) $ asks (X.terminal . X.config) >>= fst . flip run options
  where
    options = join [["-name",nm] | nm <- maybeToList termName]
      ++ join [["-cd",dir] | dir <- maybeToList termCWD]
      ++ ["-hold" | termHold] ++ termOpts
      ++ join ["-e":prog:args | prog /= ""]

-- ** X

-- | Modified from H.EWMH.fullscreenEventHook to refresh on sink.
myFullscreenEventHook :: Event -> X All
myFullscreenEventHook (ClientMessageEvent _ _ _ dpy win typ (action:dats)) = do
  wmstate <- getAtom "_NET_WM_STATE"
  fullsc <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] `fmap` getProp32 wmstate win
  let isFull = fromIntegral fullsc `elem` wstate
      -- Constants for the _NET_WM_STATE protocol:
      remove, add, toggle, ptype :: Num a => a
      remove = 0
      add = 1
      toggle = 2
      ptype = 4 -- The atom property type for changeProperty
      chWstate :: ([Foreign.C.CLong] -> [Foreign.C.CLong]) -> X ()
      chWstate f = io $ changeProperty32 dpy win wmstate ptype propModeReplace (f wstate)
  when (typ == wmstate && fi fullsc `elem` dats) $ do
    when (action == add || (action == toggle && not isFull)) $ do
      chWstate (fi fullsc:)
      windows $ W.float win $ W.RationalRect 0 0 1 1
    when (action == remove || (action == toggle && isFull)) $ do
      chWstate $ Data.List.delete (fi fullsc)
      windows $ W.sink win
      refresh -- XXX: added
  return $ All True
myFullscreenEventHook _ = return $ All True

-- | This is a re-implementation of DynWS.withNthworkspace, filters out boring WS's.
withNthWorkspace :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWorkspace job wnum = do
    sort <- DynWSO.getSortByOrder
    ws <- gets (map W.tag . sort . W.workspaces . windowset)
    case drop wnum ws of
        (w:_) -> windows $ job w
        []    -> return ()

screenAction :: (WorkspaceId -> WindowSet -> WindowSet) -> ScreenId -> X ()
screenAction f = screenWorkspace >=> flip whenJust (windows . f)

cycleRecentHiddenWS :: [KeySym] -> KeySym -> KeySym -> X ()
cycleRecentHiddenWS = A.CycleRecentWS.cycleWindowSets
    $ \w -> map (flip W.view w . W.tag) (W.hidden w ++ [W.workspace $ W.current w])

greedyViewScreen :: A.PScreens.PhysicalScreen -> X ()
greedyViewScreen ps = A.PScreens.getScreen def ps >>= flip whenJust (screenAction W.greedyView)

-- | Toggle minimize/maximize of a window. See "XMonad.Actions.Minimize".
-- first argument: maybe: only max/min True/False
toggleWindow :: Maybe Bool -> Window -> X ()
toggleWindow ma w = hiddenThisWS >>= \case
    (False, False) | ma /= Just False -> windows (W.currentTag >>= flip W.shiftWin w)
    (True,  False) | ma /= Just False -> modifyWindowSet (W.currentTag >>= flip W.shiftWin w) >> A.Minimize.maximizeWindowAndFocus w
    (True,  True)  | ma /= Just False -> modifyWindowSet (W.insertUp w . W.delete' w)         >> A.Minimize.maximizeWindowAndFocus w
    (False, True)  | ma /= Just True -> minimize
    _ -> return ()
  where
    hiddenThisWS = liftM2 (,) (runQuery isHidden w) (withWindowSet (return . elem w . currentWindows))
    -- Minimize & shift to master, so that we won't bump into the minimized window when refocusing after dead window.
    minimize = A.Minimize.minimizeWindow w >> windows (W.peek >>= \w' -> maybe id W.focusWindow w' . W.shiftMaster . W.focusWindow w)

removeNoVisibleWorkspace :: X () -> X ()
removeNoVisibleWorkspace f = withWindowSet $ \wset ->
    whenX (and <$> mapM (runQuery isHidden) (currentWindows wset)) $ DynWS.removeWorkspace >> DynWSO.removeName (W.currentTag wset) >> f

centerOnScreen :: Window -> X ()
centerOnScreen w = gets (screenRect . W.screenDetail . W.current . windowset) >>= \sr ->
    A.FloatKeys.keysMoveWindowTo (rect_x sr + round (rect_width sr % 2), rect_y sr + round (rect_height sr % 2)) (0.5, 0.5) w

maximizeRestoreTiled :: Window -> X ()
maximizeRestoreTiled w = modifyWindowSet (W.sink w) >> sendMessage (L.Maximize.maximizeRestore w)

isHidden :: Query Bool
isHidden = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_HIDDEN"

-- | Windows in the focused workspace
currentWindows :: WindowSet -> [Window]
currentWindows = W.integrate' . W.stack . W.workspace . W.current

-- ** IO

-- | Maps virtual screens to physical screens so that virtual order matches the physical order.
mapUniqueScreensPhysical :: IO [(ScreenId, A.PScreens.PhysicalScreen)]
mapUniqueScreensPhysical =
    Exception.bracket (openDisplay "") closeDisplay Graphics.X11.Xinerama.getScreenInfo
    >>= \rects -> return (zip (f rects) [A.PScreens.P 0..])
  where f rects = map fst
          $ let A.PScreens.ScreenComparator cmp = def in Data.List.sortBy cmp
          $ Data.List.nubBy (\r l -> snd r == snd l)
          $ filter (\(_,x) -> not $ any (containedIn x) rects) (zip [0..] rects)

systemdEscape :: MonadIO m => String -> m String
systemdEscape s = takeWhile (/='\n') <$> runWithInput "systemd-escape" ["--", s] ""

onIOError :: IOError -> IO ()
onIOError e = logInfo ("warning: IOError: " ++ show e)

logInfo :: MonadIO m => String -> m ()
logInfo = liftIO . putStrLn
