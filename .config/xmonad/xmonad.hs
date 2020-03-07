{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RebindableSyntax          #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : XMonad configuration
-- Copyright   : (c) 2011-2019 Samuli Thomasson
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : non-portable
--
-- https://specifications.freedesktop.org/wm-spec/wm-spec-latest.html#idm45207712345936
--
------------------------------------------------------------------------------

module Main (main) where

import qualified XMonad                              as X
import qualified XMonad.StackSet                     as W

import qualified XMonad.Actions.CopyWindow           as CW
import           XMonad.Actions.CycleRecentWS        (cycleWindowSets)
import           XMonad.Actions.CycleWS              (WSType(..))
import qualified XMonad.Actions.CycleWS              as CycleWS
import qualified XMonad.Actions.DynamicWorkspaces    as DynWS
import qualified XMonad.Actions.FlexibleManipulate   as Flex
import qualified XMonad.Actions.FloatKeys            as FloatKeys
import qualified XMonad.Actions.FloatSnap            as FloatSnap
import qualified XMonad.Actions.GridSelect           as GS
import           XMonad.Actions.Minimize             (maximizeWindowAndFocus, minimizeWindow)
import qualified XMonad.Actions.Navigation2D         as Navigation2D
import           XMonad.Actions.NoBorders            (toggleBorder)
import           XMonad.Actions.OnScreen             (Focus(..), onScreen)
import           XMonad.Actions.PhysicalScreens      (PhysicalScreen, ScreenComparator)
import qualified XMonad.Actions.PhysicalScreens      as PScreen
import qualified XMonad.Actions.RotSlaves            as RotSlaves
import qualified XMonad.Actions.SpawnOn              as SpawnOn
import qualified XMonad.Actions.UpdatePointer        as A (updatePointer)
import qualified XMonad.Actions.WorkspaceNames       as WSNames
import           XMonad.Config.Prime                 hiding (spawn, (>>))
import           XMonad.Config.Prime                 (Prime, addLayout, modifyLayout, resetLayout)
import qualified XMonad.Config.Prime                 as Arr ((>>))
import qualified XMonad.Hooks.DebugEvents            as DebugEvents
import qualified XMonad.Hooks.DebugStack             as DebugStack
import qualified XMonad.Hooks.EwmhDesktops           as EWMH
import           XMonad.Hooks.FadeWindows            (isFloating)
import           XMonad.Hooks.FloatNext              (floatNextHook, toggleFloatAllNew, willFloatAllNew, willFloatNext)
import           XMonad.Hooks.ManageDocks            (ToggleStruts(..), avoidStruts, docksEventHook, docksStartupHook, manageDocks)
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Minimize               (minimizeEventHook)
import           XMonad.Hooks.Place                  (placeFocused, placeHook, simpleSmart, smart, underMouse, withGaps)
import           XMonad.Hooks.PositionStoreHooks     (positionStoreEventHook, positionStoreManageHook)
import           XMonad.Hooks.UrgencyHook            (BorderUrgencyHook(..), focusUrgent)
import qualified XMonad.Hooks.UrgencyHook            as Urgency
import           XMonad.Hooks.WallpaperSetter
import qualified XMonad.Layout.BinarySpacePartition  as BSP
import           XMonad.Layout.BoringWindows         (boringWindows)
import qualified XMonad.Layout.BoringWindows         as BW
import qualified XMonad.Layout.Fullscreen            as FS
import           XMonad.Layout.GridVariants          (ChangeGridGeom(..), ChangeMasterGridGeom(..), Grid(Grid), SplitGrid(..))
import qualified XMonad.Layout.GridVariants          as GridV (Orientation(..))
import           XMonad.Layout.LayoutHints           (hintsEventHook)
import qualified XMonad.Layout.LayoutHints           as LayoutHints
import           XMonad.Layout.LayoutModifier        (ModifiedLayout(..))
import           XMonad.Layout.Magnifier             (magnifierOff)
import qualified XMonad.Layout.Magnifier             as Magnifier
import           XMonad.Layout.Maximize              (maximizeRestore, maximizeWithPadding)
import           XMonad.Layout.Minimize              (minimize)
import qualified XMonad.Layout.Mosaic                as Mosaic
import           XMonad.Layout.MultiToggle           (Toggle(Toggle))
import qualified XMonad.Layout.MultiToggle           as MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import qualified XMonad.Layout.NoBorders             as NoBorders
import           XMonad.Layout.OneBig                (OneBig(OneBig))
import           XMonad.Layout.Reflect               (REFLECTX(..), REFLECTY(..))
import qualified XMonad.Layout.Renamed               as Layout.Renamed
import           XMonad.Layout.Spacing               (toggleScreenSpacingEnabled, toggleWindowSpacingEnabled)
import           XMonad.Layout.Spacing               (Border(Border), spacingRaw)
import           XMonad.Layout.ThreeColumns          (ThreeCol(ThreeColMid))
import qualified XMonad.Layout.WindowArranger        as WA
import           XMonad.Layout.WindowNavigation      (Navigate(..), configurableNavigation, navigateColor)
import qualified XMonad.Prompt                       as XP
import qualified XMonad.Prompt.AppLauncher           as XP (launchApp)
import           XMonad.Prompt.ConfirmPrompt         (confirmPrompt)
import qualified XMonad.Prompt.Directory             as XP (directoryPrompt)
import           XMonad.Prompt.Input                 (inputPrompt, inputPromptWithCompl, (?+))
import qualified XMonad.Prompt.Input                 as XP.Input
import qualified XMonad.Prompt.Pass                  as XP.Pass
import qualified XMonad.Prompt.Shell                 as XP.Shell
import qualified XMonad.Prompt.Window                as XP (WindowPrompt(Goto), allWindows, windowPrompt)
import qualified XMonad.Prompt.Workspace             as XP (Wor(Wor))
import qualified XMonad.Util.ExtensibleState         as XS
import           XMonad.Util.NamedActions            (NamedAction, showKm)
import qualified XMonad.Util.PositionStore           as PosStore
import           XMonad.Util.PureX
import qualified XMonad.Util.Rectangle               as Rect
import           XMonad.Util.Types                   (Direction1D(..), Direction2D(..))
import           XMonad.Util.Ungrab                  (unGrab)
import qualified XMonad.Util.WindowProperties        as WinProp
import           XMonad.Util.WorkspaceCompare        (WorkspaceCompare, WorkspaceSort)



import           Control.Monad                       (MonadPlus(mzero), join, (>=>))
import qualified Control.Monad
import           Data.Char                           (isNumber)
import           Data.Data                           (Data)
import qualified Data.List                           as L
import qualified Data.Map                            as M
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Ratio                          ((%))
import qualified System.Environment
import           System.Exit                         (exitSuccess)
import           System.FilePath                     ((</>))
import qualified System.Posix                        as Posix
import           Text.Printf                         (printf)

import qualified System.Posix.Process                as Proc

import           MyRun
import           MyTheme
import qualified MyXmobar
import           Scratchpads
import           XMonad.Config.CommandsKeysF
import qualified XMonad.Config.CommandsKeysF         as CF
import qualified XMonad.Prompt.Qutebrowser           as XP.QB
import qualified XMonad.Util.DesktopNotifications    as Notify
import           XMonad.Util.NamedCommands

main :: IO ()
main = xmonad configPrime

configPrime :: _ => Prime l _
configPrime = do
  terminal           =: "urxvtc"
  borderWidth        =: 1
  focusedBorderColor =: colCyan
  normalBorderColor  =: colBase02
  clientMask         =+ focusChangeMask -- default: structureNotifyMask .|. enterWindowMask .|. propertyChangeMask@
  -- rootMask -- default: substructureRedirectMask .|. substructureNotifyMask .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask .|. buttonPressMask
  modMask            =: mod4Mask
  -- mouseBindings      =+ myMouseBindings
  focusFollowsMouse  =: True
  clickJustFocuses   =: False
  manageHook         =: myManageHook
  myLayout
  applyIO $ CF.addAll myShowKeys myCmds
  MyXmobar.myStatusBars
  urgencyHook (BorderUrgencyHook colGreen)
  urgencyHook Notify.urgencyHook
  startupHook        =+ Notify.startupHook
  startupHook        =+ EWMH.ewmhDesktopsStartup   -- Initialize and advertise EWMH support
  startupHook        =+ EWMH.fullscreenStartup
  startupHook        =+ docksStartupHook
  handleEventHook    =+ EWMH.ewmhDesktopsEventHook -- Intercepts _NET_CURRENT_DESKTOP, _NET_WM_DESKTOP, _NET_ACTIVE_WINDOW
  handleEventHook    =+ FS.fullscreenEventHook
  handleEventHook    =+ debugEventHook
  handleEventHook    =+ positionStoreEventHook
  handleEventHook    =+ minimizeEventHook    -- Handle minimize/maximize requests
  handleEventHook    =+ hintsEventHook       -- Refreshes the layout whenever a window changes its hints.
  handleEventHook    =+ docksEventHook
  handleEventHook    =+ myRestartEventHook
  logHook            =+ EWMH.ewmhDesktopsLogHookCustom id
  logHook            =+ myUpdatePointer (0.5, 0.5) (0.4, 0.4)
  logHook            =+ wallpaperSetter myWallpaperConf
  where
    (>>) = (Arr.>>)

    urgencyHook :: (Urgency.UrgencyHook h, LayoutClass l Window) => h -> Prime l l
    urgencyHook = apply . flip Urgency.withUrgencyHookC Urgency.urgencyConfig { Urgency.suppressWhen = Urgency.Focused }

myLayout :: LayoutClass l Window => Prime l _
myLayout = do
  -- NOTE: resetLayout sets the head, addLayout adds to the right of (|||)
  resetLayout BSP.emptyBSP
  -- SplitGrid (master pos) (master rows) (master cols) (master portion) (slaves x:y aspect) (resize increment)
  addLayout (SplitGrid GridV.T 1 2 (11/18) (4/3) (5/100))
  addLayout (Grid (16/9))
  addLayout (ThreeColMid 1 (1/30) (4/9))
  addLayout (Tall 1 (3/100) (1/2))
  addLayout (OneBig (2/3) (2/3))
  addLayout (Mosaic.mosaic 1.25 [3,2])
  --addLayout (Mosaic.mosaic 1.5 [])
  -- addLayout positionStoreFloat

  -- NOTE: MIRROR with REFLECTX/Y is most intuitive when mirror goes first.
  modifyLayout (MultiToggle.mkToggle1 MIRROR)
  modifyLayout (MultiToggle.mkToggle1 REFLECTX)
  modifyLayout (MultiToggle.mkToggle1 REFLECTY)
  modifyLayout (MultiToggle.mkToggle1 NOBORDERS)
  modifyLayout (MultiToggle.mkToggle1 HINT)

  -- NOTE: WindowNavigation interacts badly with some modifiers like "maximize" and "spacing", apply those after it.
  modifyLayout (configurableNavigation $ navigateColor colBase00)

  -- border/spacing/maximize/magnify
  modifyLayout (mySpacing 1 2)

  modifyLayout (maximizeWithPadding 80)
  modifyLayout magnifierOff
  -- NOTE: Apply avoidStruts late so that other modifiers aren't affected.
  modifyLayout avoidStruts
  -- Fullscreen _NET_WM_STATE_FULLSCREEN layout support.
  modifyLayout FS.fullscreenFull
  -- NOTE: This replaces the layout, including modifiers applied before it.
  modifyLayout (MultiToggle.mkToggle1 NBFULL)
  -- Less borders
  modifyLayout (NoBorders.lessBorders MyAmbiguity)
  -- minimize/boring
  modifyLayout boringWindows
  modifyLayout minimize

  -- experimenting
  modifyLayout WA.windowArrange
  where
    (>>) = (Arr.>>)

    mySpacing sd wd = spacingRaw True (f sd) True (f wd) True
      where f n = Border n n n n

-- appName =? "dialog" -- urxvtc -name dialog
-- default: insertPosition Above Newer
myManageHook :: ManageHook
myManageHook = composeAll
  [ positionStoreManageHook Nothing
  , composeOne $
      scratchpadsManageHook myScratchpads ++
    [ transience
    , className =? "feh"             -?> smartPlaceHook (16,5,16,5) (0,0) <+> doFloat
    , className =? "Xmessage"        -?> doSideFloat SW
    , className =? "Xmag"            -?> doSideFloat NC
    , className =? "Nvidia-settings" -?> doCenterFloat
    , isDialog                       -?> placeHook (underMouse (0.7,0.7)) <+> doFloat
    , definiteToMaybe $ smartPlaceHook (30,30,30,30) (0.5,0.5)
    ]
  , isFloating =? False <&&> anyWindowCurWS isFullscreen --> doFloat
  , FS.fullscreenManageHookWith makeFullscreen
  , SpawnOn.manageSpawn
  , floatNextHook
  , manageDocks
  ]
  where
    makeFullscreen = className =? "Google-chrome" <&&> appName =? "netflix.com"

    -- inverse of X.H.ManageHelpers.maybeToDefinite
    definiteToMaybe = fmap Just

    smartPlaceHook gaps pos = placeHook (withGaps gaps (smart pos))

    anyWindowCurWS f = liftX $ any id <$> (getStack >>= mapM (runQuery f) . W.integrate')

-- | Modified from XMonad.Main.handle
myRestartEventHook :: Event -> X All
myRestartEventHook e@ClientMessageEvent { ev_message_type = mt } = whenM' (fmap (mt ==) (getAtom "XMONAD_RESTART")) (myRestart Control.Monad.>> mempty)
myRestartEventHook _                                             = mempty

myRecompileRestart :: Bool -> Bool -> X ()
myRecompileRestart rcFlag rsFlag = do
  Notify.notifyLastS "Recompiling"
  _p <- xfork $ whenM' (recompile rcFlag) $ when' rsFlag $ myXMonad ["--restart"]
  --Notify.notifyLastS "Recompile success"
  return ()
  where
    (>>) = (Control.Monad.>>)

myRestart :: X ()
myRestart = do
  dir  <- getXMonadDataDir
  prog <- io System.Environment.getProgName
  let msg = printf "Restart (%s)..." prog
  trace msg
  Notify.notifyLastS msg
  Notify.exitHook
  MyXmobar.exitHook
  restart (dir </> prog) True
  where
    (>>) = (Control.Monad.>>)

myXMonad :: MonadIO m => [String] -> m ()
myXMonad args = do
  dir  <- getXMonadDataDir
  prog <- io System.Environment.getProgName
  spawn $ program (dir </> prog) args
  where
    (>>) = (Control.Monad.>>)

myWallpaperConf :: WallpaperConf
myWallpaperConf = def
  { wallpaperBaseDir = "images/wallpapers"
  , wallpapers       = WallpaperList [(ws,WallpaperDir "3840x2160") | ws <- show <$> [1..20::Int]]
  }

myScratchpads :: [Scratchpad]
myScratchpads = mkXScratchpads
  [("tmux-0",     tmux (Just "0"),                                  appName =? "tmux-0")
  ,("ncmpcpp",    spawn $ inTerm (tName "ncmpcpp")    "ncmpcpp",    appName =? "ncmpcpp")
  ,("pulsemixer", spawn $ inTerm (tName "pulsemixer") "pulsemixer", appName =? "pulsemixer")
  ]
  (doRectFloat (W.RationalRect 0.2 0.1 0.6 0.6))

-- Modified to not fire on spammy property updates (e.g. status bar stuff).
myUpdatePointer :: _ -> _ -> X ()
myUpdatePointer x y = whenX (check <$> asks currentEvent) (A.updatePointer x y)
  where check (Just PropertyEvent{}) = False
        check _                      = True

willFloat :: Query Bool
willFloat = liftX (willFloatNext <||> willFloatAllNew)

maximizeRestore' :: Window -> X ()
maximizeRestore' w =
  runQuery isFullscreen w >>= \case
    True  -> unFullscreen w
    False -> runQuery isFloating w >>= \case
      True  -> windows (W.sink w)
      False -> sendMessage (maximizeRestore w)

unFullscreen :: Window -> X ()
unFullscreen w = do
  a_st <- getAtom "_NET_WM_STATE"
  a_fs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  wstate <- fromMaybe [] <$> WinProp.getProp32 a_st w
  withDisplay $ \dpy ->
    io $ changeProperty32 dpy w a_st aTOM propModeReplace (L.delete (fromIntegral a_fs) wstate)
  broadcastMessage (FS.RemoveFullscreen w)
  sendMessage FS.FullscreenChanged
    where
      (>>) = (Control.Monad.>>)

centerOnScreen' :: Window -> X ()
centerOnScreen' win =
  curScreen >>= \W.Screen{screenDetail=SD sr@(Rectangle x y w h)} ->
  runQuery isFloating win >>= \isf ->
  withDisplay $ \dpy ->
  io (getWindowAttributes dpy win) >>= \WindowAttributes{..} ->
    let wr  = Rectangle (fromIntegral wa_x) (fromIntegral wa_y) (fromIntegral wa_width) (fromIntegral wa_height)
        ((a,b),(c,d)) = (Rect.center sr, Rect.center wr)
     in if isf && abs (c-a) + abs (d-b) < 5
           then windows (W.sink win) -- reset
           else FloatKeys.keysMoveWindowTo (x+round (w%2), y+round (h%2)) (0.5,0.5) win

myShowKeys :: CF.ShowKeys
myShowKeys ts xs = Notify.notify_ $ Notify.summary (unwords ("Keys":ts)) $ Notify.body ("<tt>" ++ unlines (showKm xs) ++ "</tt>") def

type LayoutCommand = "Layout" :??
  '[ SetLayoutCmd
   , SendMessage ChangeLayout
   , SendMessage Resize
   , SendMessage IncMasterN
   , SendMessage ToggleStruts
   , SendMessage Magnifier.MagnifyMsg
   , Mosaic.Aspect
   , Toggle StdTransformers
   , Toggle HINT
   , Toggle REFLECTX
   , Toggle REFLECTY
   , WA.WindowArrangerMsg
   ]

type LayoutBSPCommand = "BSP" :??
  '[SendMessage BSP.TreeRotate,
    SendMessage BSP.TreeBalance,
    SendMessage BSP.ResizeDirectional,
    SendMessage BSP.Rotate,
    SendMessage BSP.Swap,
    SendMessage BSP.FocusParent,
    SendMessage BSP.SelectMoveNode,
    SendMessage BSP.SplitShiftDirectional
   ]

type LayoutGridCommand = "Grid" :??
  '[ ChangeGridGeom
   , ChangeMasterGridGeom
   , SendMessage Resize
   ]

type WindowCommand = "Window" :?? '[ WindowCmd, Navigate ]

type FloatCommand = "Floating" :?? '[ MyFloatCmd ]

data SetLayoutCmd = ResetLayout
                  | MaximizeRestore
                  | ToggleScreenSpacing
                  | ToggleWindowSpacing
                  deriving (Show, Data)

data MyFloatCmd = CenterWindow
                | SinkWindow
                | SwitchLayer
                | SnapMove   Direction2D (Maybe Int)
                | SnapGrow   Direction2D (Maybe Int)
                | SnapShrink Direction2D (Maybe Int)
                | PlaceSimpleSmart
                | ToggleFloatAllNew
                deriving (Show,Data)

data MyFloatMouseCmd = FloatMouseFlexDiscrete Window
                     | FloatMouseFlexResize   Window
                     deriving (Show,Data)

data WindowCmd = FocusSwapMaster Window
               | WindowKill1
               | WindowKillPID
               | FocusMaster
               | FocusUp
               | FocusDown
               | FocusUrgent
               | SwapMaster
               | SwapUp
               | SwapDown
               | RotSlavesDown
               | RotSlavesUp
               | RotAllDown
               | RotAllUp
               | ToggleFocusedWindowBorder
               deriving (Show, Data)

{-
data ScreenCmd
  = CycleScreenWS CycleWSCmd
  | ViewScreen WorkspaceId
-}

data WorkspaceCmd = WorkspaceOnScreen Focus PhysicalScreen -- greedyview
                  | WorkspaceView Int -- DynWS.withNthWorkspace / W.greedyView
                  | WorkspaceCopy Int -- DynWS.withNthWorkspace / CW.copy
                  | WorkspaceShiftTo Int -- DynWS.withNthWorkspace / W.shift
                  | WorkspaceCycleRecentHidden -- cycleRecentHiddenWS
                  | WorkspaceAddPrompt -- wsPromptNew' / DynWS.addWorkspace
                  | WorkspaceSetNamePrompt -- WSNames
                  | WorkspaceRemoveFocused              -- removeNoVisibleWS
                  | WorkspaceSwapTo Direction1D WSType  -- WSNames
                  | WorkspaceSendToScreen PhysicalScreen
                  | WorkspaceViewScreen PhysicalScreen
                  | FocusScreenIn Direction1D

instance IsCmd WorkspaceCmd where
  command (WorkspaceOnScreen focus ps@(PScreen.P s')) = PScreen.getScreen    def ps ?+ (\s -> windows (W.currentTag >>= \x -> onScreen (W.greedyView x) focus s)) ? printf "View screen %i (%s)" s' (show focus)
  command (WorkspaceSendToScreen   ps@(PScreen.P s')) = PScreen.sendToScreen def ps ? printf "Send workspace to screen %i" s'
  command (WorkspaceViewScreen     ps@(PScreen.P s')) = PScreen.viewScreen   def ps ? printf "View screen %i"              s'
  command (WorkspaceView    ws)  = DynWS.withNthWorkspace W.greedyView ws ? printf "View tag %i" ws
  command (WorkspaceCopy    ws)  = DynWS.withNthWorkspace CW.copy      ws ? printf "Copy focused to tag %i" ws
  command (WorkspaceShiftTo ws)  = DynWS.withNthWorkspace W.shift      ws ? printf "Move focused to tag %i" ws
  -- TODO keys
  command WorkspaceCycleRecentHidden = cycleRecentHiddenWS [xK_Super_L, xK_Alt_L] xK_y xK_p ? "Cycle (focus) recent tags"
  -- TODO DynWS + WSNames
  command WorkspaceAddPrompt     = wsPromptNew' "Add tag: " ?+ DynWS.addWorkspace ? "New tag (XP)"
  command WorkspaceSetNamePrompt = wsPromptNew' "Rename tag: " ?+ WSNames.setCurrentWorkspaceName ? "Rename this tag (XP)"
  command WorkspaceRemoveFocused = removeNoVisibleWS ? "Remove this tag (if empty)"
  command (WorkspaceSwapTo d _)  = WSNames.swapTo d ? printf "Shift current tag %s" (if d == Next then "forward" else "backwards")
  command (FocusScreenIn Next)   = CycleWS.nextScreen ? "Focus next screen"
  command (FocusScreenIn Prev)   = CycleWS.prevScreen ? "Focus previous screen"

  cmdEnum _ = [WorkspaceAddPrompt,WorkspaceCycleRecentHidden,WorkspaceRemoveFocused]

--deriving instance Show WSType
--deriving instance Read WSType
--deriving instance Data WSType

deriving instance Data PhysicalScreen
deriving instance Data Focus
deriving instance Show Focus
deriving instance Read Focus

-- X.A.OnScreen.OnScreen:
-- | FocusNew
-- | FocusCurrent
-- | FocusTag WorkspaceId
-- | FocusTagVisible WorkspaceId

-- TODO
data CycleWSCmd = CycleWSCmd Direction1D CycleWS.WSType CycleWSAction WorkspaceSort
-- TODO
data CycleWSAction = CWSMoveTo | CWSView | CWSSwap

{-
cycleRecentHiddenWS :: [KeySym] -> KeySym -> KeySym -> X ()
cycleWindowSets     :: ([WindowSet]->[WindowSet]) -> [KeySym] -> KeySym -> KeySym -> X ()

CycleWindows:
  cycleRecentWindows :: [KeySym] -> KeySym -> KeySym -> X ()
  cycleStacks'       :: (W.Stack Window->[W.Stack Window]) -> KeySym -> KeySym -> KeySym -> X ()
  rotDown
  rotFocused
  rotFocusedDown
  rotFocusedUp
  rotOpposite
  rotOpposite' :: W.Stack a -> W.Stack a
  rotUnfocused' :: ([a] -> [a]) -> W.Stack -> W.Stack -> W.Stack
  rotUnfocusedDown
  rotUnfocusedUp
  rotUp
  shiftToFocus' :: (Eq a, Show a, Read a) a -> W.Stack a -> W.Stack a
-}

instance IsCmd SetLayoutCmd where
  command ResetLayout         = (asks (X.layoutHook . X.config) >>= setLayout) ? "Reset layout"
  command MaximizeRestore     = withFocused maximizeRestore'                   ? "Maximize / restore window"
  command ToggleScreenSpacing = toggleScreenSpacingEnabled                     ? "Toggle screen spacing"
  command ToggleWindowSpacing = toggleWindowSpacingEnabled                     ? "Toggle window spacing"

instance IsCmd MyFloatCmd where
  command CenterWindow      = withFocused centerOnScreen'   ? "Center on screen"
  command SinkWindow        = withFocused (windows . W.sink)? "Sink"
  command SwitchLayer       = Navigation2D.switchLayer      ? "Switch Layer (Nav2D)"
  command PlaceSimpleSmart  = placeFocused simpleSmart      ? "(Re-)place window: simpleSmart (X.H.Place)"
  command ToggleFloatAllNew = toggleFloatAllNew             ? "Toggle float new windows"
  command (SnapMove   d2 p) = withFocused (FloatSnap.snapMove   d2 p) ? printf "Move %s (FloatSnap)" (show d2)
  command (SnapGrow   d2 p) = withFocused (FloatSnap.snapGrow   d2 p) ? printf "Grow %s (FloatSnap)" (show d2)
  command (SnapShrink d2 p) = withFocused (FloatSnap.snapShrink d2 p) ? printf "Shrink %s (FloatSnap)" (show d2)

instance IsCmd MyFloatMouseCmd where
  command (FloatMouseFlexDiscrete w) = Flex.mouseWindow Flex.discrete w ? "Mouse: flexible move window (discrete)"
  command (FloatMouseFlexResize   w) = Flex.mouseWindow Flex.resize w   ? "Mouse: flexible resize window"
  cmdEnum _ = []

instance IsCmd WindowCmd where
  command WindowKill1         = CW.kill1                                                  ? "Kill (1 copy) window (X.A.CopyWindow)"
  command WindowKillPID       = withFocused (signalProcessBy Posix.sigKILL)               ? "Signal process (SIGKILL) of focused window (_NET_WM_PID)"
  command FocusMaster         = BW.focusMaster                                            ? "Focus master (BoringWindows)"
  command FocusUp             = BW.focusUp                                                ? "Focus up (BoringWindows)"
  command FocusDown           = BW.focusDown                                              ? "Focus down (BoringWindows)"
  command FocusUrgent         = focusUrgent                                               ? "Focus urgent window"
  command SwapMaster          = windows W.swapMaster                                      ? "Swap to master"
  command SwapUp              = windows W.swapUp                                          ? "Swap up"
  command SwapDown            = windows W.swapDown                                        ? "Swap down"
  command RotSlavesDown       = RotSlaves.rotSlavesDown                                   ? "Rotate slaves down"
  command RotSlavesUp         = RotSlaves.rotSlavesUp                                     ? "Rotate slaves up"
  command RotAllDown          = RotSlaves.rotAllDown                                      ? "Rotate down"
  command RotAllUp            = RotSlaves.rotAllUp                                        ? "Rotate up"
  command (FocusSwapMaster w) = windows (W.focusWindow w Control.Monad.>> W.swapMaster)   ? "Focus window & swap it master"
  command ToggleFocusedWindowBorder = (withFocused toggleBorder Control.Monad.>> refresh) ? "Toggle focused window border"

deriving instance Show WA.WindowArrangerMsg

instance IsCmd WA.WindowArrangerMsg where
  command msg = sendMessage msg ? (show msg <> " (WA)")
  cmdEnum _ = [WA.Arrange, WA.DeArrange] ++
    [ WA.IncreaseLeft  i
    , WA.IncreaseRight i
    , WA.IncreaseDown  i
    , WA.IncreaseUp    i
    , WA.DecreaseLeft  i
    , WA.DecreaseRight i
    , WA.DecreaseDown  i
    , WA.DecreaseUp    i
    ] where i = 25

instance IsCmd Mosaic.Aspect where
  command x@Mosaic.Wider      = sendMessage x ? "MOSAIC Wider"
  command x@Mosaic.Taller     = sendMessage x ? "MOSAIC Taller"
  command x@Mosaic.Reset      = sendMessage x ? "MOSAIC Reset"
  command x@Mosaic.SlopeMod{} = sendMessage x ? "MOSAIC Slope ..."
  cmdEnum _ = [Mosaic.Taller, Mosaic.Wider, Mosaic.Reset]

deriving instance Show ChangeGridGeom

instance IsCmd ChangeGridGeom where
  cmdEnum _ = concat
    [[SetGridAspect    r | r <- []]
    ,[ChangeGridAspect r | r <- [2/100,-2/100]]]

deriving instance Show ChangeMasterGridGeom

instance IsCmd ChangeMasterGridGeom where
  cmdEnum _ = concat
    [[IncMasterRows i | i <- [1,-1]]
    ,[IncMasterCols i | i <- [1,-1]]
    ,[SetMasterRows i | i <- [1,-1]]
    ,[SetMasterCols i | i <- [1,-1]]
    ,[SetMasterFraction r|r<-[]] ]

-- ChangeGridGeom

-- Kensington button keycodes:
--  bottom-left:  1 left
--  top-left:     2 middle
--  bottom-right: 3 right
--  scroll wheel: 4 5
--  top-right:    8 back

myCmds :: (LayoutClass l Window, Read (l Window)) => CF.Cmd l ()
myCmds = CF.hinted "Commands" 1 $ \helpCmd -> do

  let unPScreen (PScreen.P s) = s
      onPScreen f g a ps = PScreen.getScreen def ps ?+ \s -> windows (g >>= \x -> onScreen (f x) a s)

      skeys        = zip screenKeys [PScreen.P 0 ..]
      tags         = zip tagKeys [(0::Int)..]
      tagKeys      = map (:[]) ['a'..'z']
      screenKeys   = map (:[]) "wvz"

      volume :: Int -> _
      volume d = spawn "pactl" ["set-sink-volume",   "@DEFAULT_SINK@",   printf "%+i%%" d] ? printf "Volume %+i%%" d

      mic :: Int -> _
      mic d = spawn "pactl" ["set-source-volume", "@DEFAULT_SOURCE@", printf "%+i%%" d] ? printf "Volume (mic) %+i%%" d

      toggleMuteSource :: _
      toggleMuteSource = spawn "pactl" ["set-source-mute", "@DEFAULT_SOURCE@", "toggle"] ? "Toggle mute (default source)"

      toggleMuteSink :: _
      toggleMuteSink = spawn "pactl" ["set-sink-mute", "@DEFAULT_SINK@", "toggle"] ? "Toggle mute (default sink)"

      backlight :: Int -> _
      backlight d = spawn "xbacklight" [if d >= 0 then "-inc" else "-dec", printf "%i" (abs d)] ? printf "Backlight %+i%%" d

      mpc cmd          = spawn "mpc" [cmd] ? printf "MPD: %s" cmd
      xmag             = spawn "xmag" ["-mag","2","-source","960x540"] ? "xmag"
      bluetoothctl     = spawn (inTerm dialog "bluetoothctl") ? "bluetoothctl"
      mpvWithClipboard = spawn (sdRun "" "" $ shell "exec mpv --really-quiet --profile=preview \"$(xclip -o)\" &>/dev/null") ? "mpv: xclip -o"
      clipmenu         = spawn "clipmenu" ["-p","clipmenu","-i"] ? "clipmenu"

  let togglePad :: String -> NamedAction
      togglePad pad = toggleScratchpad myScratchpads pad ? printf "Toggle %s (pad)" pad

  group "Mouse" $ CF.modDef $ \modm -> do
    (modm,               button1) /+ FloatMouseFlexDiscrete
    (modm .|. shiftMask, button1) /+ FloatMouseFlexResize
    (modm,               button2) /+ FocusSwapMaster
    (modm,               button1) /+ FloatMouseFlexDiscrete

  group "XMonad & X11" $ do
    "M-<F1>" `CF.key'` helpCmd
    "M-<Return>" >+ spawn (term def) ? "Terminal"
    "M-S-c"      >+ WindowKill1
    "M-$"        >+ spawn (sdRun "physlock" "" "physlock") ? "Lock (physlock)"
    "M-<Esc>"    >+ debugWindowSet                         ? "Debug: WindowSet"
    "M-q"        >+ myRecompileRestart False True          ? "recompile && restart"
    "M-C-q"      >+ myRecompileRestart True False          ? "Recompile (force)"
    "M-S-q"      >+ io exitSuccess                         ? "Exit"

  group "Command prompts" $ do
    "M-r M-c" >+ promptCommand xpConfig
    "M-b M-l" >+ cmdPrompt xpConfig (Proxy :: Proxy LayoutCommand)
    "M-b M-w" >+ cmdPrompt xpConfig (Proxy :: Proxy WindowCommand)
    "M-b M-f" >+ cmdPrompt xpConfig (Proxy :: Proxy FloatCommand)
    "M-b M-r" >+ cmdPrompt xpConfig (Proxy :: Proxy LayoutBSPCommand)
    "M-b M-g" >+ cmdPrompt xpConfig (Proxy :: Proxy LayoutGridCommand)

  group "Layout" $ do
    "M-C-<Space>" >+ ResetLayout
    "M-<Space>"   >+ SendMessage NextLayout
    "M-S-<Space>" >+ SendMessage FirstLayout
    "M-x"         >+ SendMessage Shrink
    "M-S-x"       >+ SendMessage Expand
    "M-."         >+ SendMessage (IncMasterN 1)    :>> Mosaic.Taller
    "M-,"         >+ SendMessage (IncMasterN (-1)) :>> Mosaic.Wider
    "M-b t"       >+ SendMessage ToggleStruts
    "M-b l"       >+ SendMessage Magnifier.Toggle
    "M-m"         >+ MaximizeRestore
    "M-b s"       >+ ToggleScreenSpacing :>> ToggleWindowSpacing
    "M-b b"       >+ Toggle NOBORDERS
    "M-b h"       >+ Toggle HINT
    "M-b f"       >+ Toggle NBFULL
    "M-b m"       >+ Toggle MIRROR
    "M-b x"       >+ Toggle REFLECTX
    "M-b y"       >+ Toggle REFLECTY
    "M-b M-x"     >+ SendMessage ToggleStruts :>> Toggle NOBORDERS :>> ToggleScreenSpacing :>> ToggleWindowSpacing

  group "BSP" $ do
    "M-b M-u" >+ SendMessage BSP.FocusParent
    "M-b M-y" >+ SendMessage BSP.SelectNode
    "M-b M-p" >+ SendMessage BSP.MoveNode
    "M-C-"    >>+ directions2D >++> SendMessage . BSP.ExpandTowards

  group "WindowArranger" $ do
    "M-C-b a"   >+ WA.Arrange
    "M-C-b S-a" >+ WA.DeArrange
    "M-C-b h"   >+ WA.IncreaseLeft  25
    "M-C-b l"   >+ WA.IncreaseRight 25
    "M-C-b k"   >+ WA.IncreaseDown  25
    "M-C-b j"   >+ WA.IncreaseUp    25
    "M-C-b M-h" >+ WA.DecreaseLeft  25
    "M-C-b M-l" >+ WA.DecreaseRight 25
    "M-C-b M-k" >+ WA.DecreaseDown  25
    "M-C-b M-j" >+ WA.DecreaseUp    26

  group "Window" $ do
    "M-/"       >+ showHideW ? "Show/hide this window"
    "M-"        >>+ directions2D >++> Go
    "M-S-"      >>+ directions2D >++> Swap
    "M-f "      >>+ directions2D >++> flip SnapMove   Nothing
    "M-f S-"    >>+ directions2D >++> flip SnapGrow   Nothing
    "M-f C-"    >>+ directions2D >++> flip SnapShrink Nothing
    "M-f ,"     >+ RotSlavesDown
    "M-f ."     >+ RotSlavesUp
    "M-f M-,"   >+ RotAllDown
    "M-f M-."   >+ RotAllUp
    "M-f M-m"   >+ FocusMaster
    "M-f M-n"   >+ FocusUp
    "M-f M-p"   >+ FocusDown
    "M-f m"     >+ SwapMaster
    "M-f n"     >+ SwapUp
    "M-f p"     >+ SwapDown
    "M-f u"     >+ FocusUrgent
    "M-f b"     >+ ToggleFocusedWindowBorder
    "M-f c"     >+ CenterWindow
    "M-f s"     >+ SinkWindow
    "M-f y"     >+ SwitchLayer
    "M-f M-h"   >+ promptSpawnOnByWindowPPID                     ? "Shift by PPID to some workspace"
    "M-f h"     >+ (curTag >>= SpawnOn.manageByPPID 0 . doShift) ? "Shift all to this workspace"

  group "Screen" $ do
    "M-C-<Right>" >+ FocusScreenIn Prev
    "M-C-<Left>"  >+ FocusScreenIn Next
    "M-S-"        >>+ skeys >++> WorkspaceSendToScreen
    "M-"          >>+ skeys >++> WorkspaceViewScreen
    "M-M1-"       >>+ skeys >++> WorkspaceOnScreen FocusCurrent
    "M-C-"        >>+ skeys >++> WorkspaceOnScreen FocusNew

  group "Workspaces" $ do
    "M-; "        >>+ tags >++> WorkspaceView
    "M-; M-"      >>+ tags >++> WorkspaceCopy
    "M-S-; "      >>+ tags >++> WorkspaceShiftTo
    "M-y"         >+ WorkspaceCycleRecentHidden
    "M-S-n"       >+ WorkspaceSwapTo Next AnyWS
    "M-S-p"       >+ WorkspaceSwapTo Prev AnyWS
    "M-g r"       >+ WorkspaceSetNamePrompt
    "M-g n"       >+ WorkspaceAddPrompt
    "M-g d"       >+ WorkspaceRemoveFocused
    "M-g S-n"     >+ wsPromptNew' "New tag for window: " ?+ ((\to -> DynWS.addHiddenWorkspace to >> defile (shift to))) ? "Move window to new tag (XP)"
    "M-g c"       >+ wsPrompt'    "Copy to tag: "        ?+ ((\to -> withFocii $ \_ w -> windows $ CW.copyWindow w to)) ? "Copy window to this tag (XP)"
    "M-g m"       >+ wsPrompt'    "Shift to tag: "       ?+ (defile . shift                                           ) ? "Move window to this tag (XP)"
    "M-g g"       >+ wsPrompt'    "View tag: "           ?+ (defile . greedyView                                      ) ? "Go to tag (XP)"
    "M-g s"       >+ GS.goToSelected gsconfig1                          ? "Go to window (GS)"
    "M-g f"       >+ XP.windowPrompt xpConfigAuto XP.Goto XP.allWindows ? "Go to window (XP)"

  group "XPads" $ do
    "M-!"         >+ togglePad "tmux-0"
    "M-<Tab>"     >+ togglePad "tmux-0"
    "M-#"   >+ togglePad "ncmpcpp"
    "M-c m" >+ togglePad "pulsemixer"
    "M-r s" >+ inputPromptWithCompl xpConfig "scratchpad" (scratchpadCompl xpConfig myScratchpads) ?+ toggleScratchpad myScratchpads ? "Prompt: pad"

  group "Launchers" $ do
    "M-r M-S-c"    >+ WindowKillPID
    "M-r t"        >+ tmux Nothing ? "Terminal (tmux)"
    "M-r <Return>" >+ inputPromptProg xpConfig "Shell"    ?+ (spawn . sdRun "" "" . shell) ? "Prompt: shell"
    "M-r r"        >+ inputPromptProg xpConfig "Service"  ?+ (\xs -> spawn (sdRun "" "" (head $ words xs, tail $ words xs))) ? "Prompt: service"
    "M-r M-r"      >+ inputPromptProg xpConfig "Terminal" ?+ (spawn . inTerm tHold . shell) ? "Prompt: shell (in terminal)"
    "M-r g"        >+ XP.launchApp xpConfig "gimp" ? "Prompt: gimp"
    "M-r q"        >+ XP.QB.qutebrowserP xpConfigNoHist "qutebrowser" ?+ XP.QB.qutebrowser ? "Prompt: qutebrowser"
    "M-r u"        >+ promptBrowserApp "google-chrome-stable" []

  group "Utils" $ do
    "M-r c"   >+ clipmenu
    "M-r v"   >+ mpvWithClipboard
    "M-r b"   >+ bluetoothctl
    "M-r m"   >+ xmag
    "M-r p"   >+ XP.Pass.passPrompt xpConfig          ? "Prompt: pass"
    "M-r C-p" >+ XP.Pass.passOTPPrompt xpConfig       ? "Prompt: pass: OTP"
    "M-r e"   >+ promptEnviron xpConfig ?+ setsEnvCmd ? "Prompt: SetEnv"

  group "Media" $ do
    "M-+"                     >+ volume 3
    "M--"                     >+ volume (-3)
    "M-c n"                   >+ mpc "next"
    "M-c p"                   >+ mpc "prev"
    "M-c t"                   >+ mpc "toggle"
    "M-c y"                   >+ mpc "single"
    "M-c r"                   >+ mpc "random"
    "<XF86AudioMute>"         >+ toggleMuteSink
    "<XF86AudioMicMute>"      >+ toggleMuteSource
    "<XF86AudioRaiseVolume>"  >+ volume 3
    "<XF86AudioLowerVolume>"  >+ volume (-3)
    "<XF86MonBrightnessUp>"   >+ backlight   2
    "<XF86MonBrightnessDown>" >+ backlight (-2)

  where
    (>>) = (Control.Monad.>>)

directions2D = map (:[]) "kjlh" `zip` [minBound..maxBound @Direction2D]

signalProcessBy :: Posix.Signal -> Window -> X ()
signalProcessBy s w = runQuery pid w ?+ \p ->
  confirmPrompt' (printf "kill -%i %i" (toInteger s) (toInteger p)) (io $ Posix.signalProcess s p)

cycleRecentHiddenWS :: [KeySym] -> KeySym -> KeySym -> X ()
cycleRecentHiddenWS =
  cycleWindowSets $ \wset ->
    [W.view (W.tag ws) wset | ws <- W.hidden wset ++ [W.workspace (W.current wset)]]

removeNoVisibleWS :: X ()
removeNoVisibleWS =
  curWorkspace >>= \ws ->
    whenX (fmap and $ mapM (runQuery isMinimized) (W.integrate' $ W.stack ws)) DynWS.removeWorkspace

-- * XP

confirmPrompt' :: String -> X () -> X ()
confirmPrompt' = confirmPrompt xpConfig

-- | An existing WorkspaceId
wsPrompt' :: String -> X (Maybe WorkspaceId)
wsPrompt' pstr = join . fmap (either (const Nothing) Just) <$> wsPromptWith pstr

-- | Non-existing Workspace name
wsPromptNew' :: String -> X (Maybe String)
wsPromptNew' pstr = join . fmap (either Just (const Nothing)) <$> wsPromptWith pstr

-- | An existing WorkspaceId (Right) or some other string (Left).
wsPromptWith :: String -> X (Maybe (Either String WorkspaceId))
wsPromptWith pstr = do
  ids   <- gets (map W.tag . W.workspaces . windowset)
  names <- (`map` ids) <$> WSNames.getWorkspaceNames
  XP.mkXPromptWithReturn (XP.Wor pstr) xpConfig
      (\s -> return $ filter (XP.searchPredicate xpConfig s) names)
      (\r -> return $ maybe (Left r) Right $ lookup r (zip names ids))

inputPrompt' :: String -> X (Maybe String)
inputPrompt' = inputPrompt xpConfig

inputPromptWithCompl' :: String -> XP.ComplFunction -> X (Maybe String)
inputPromptWithCompl' = XP.Input.inputPromptWithCompl xpConfig

inputPromptWithHistCompl :: XP.XPConfig -> String -> X (Maybe String)
inputPromptWithHistCompl xpc name = XP.Input.inputPromptWithCompl xpc name (XP.historyCompletionP (== name ++ ": "))

promptEnviron :: XP.XPConfig -> X (Maybe String)
promptEnviron xpc = do
  vars <- io $ System.Environment.getEnvironment
  XP.Input.inputPromptWithCompl xpc "Environment" $
    \s -> return $ filter (XP.searchPredicate xpc s) [k<>"="<>v|(k,v)<-vars]

setsEnvCmd :: String -> X ()
setsEnvCmd input
  | (k,_:v) <- span (/= '=') input =
    io (System.Environment.lookupEnv k) >>= \c ->
      confirmPrompt' (printf "set %s='%s' (current '%s')" k v (maybe "" id c)) (io (System.Environment.setEnv k v))
  | otherwise = return ()

-- | google-chrome-stable, chromium, etc. whatever with --app switch
promptBrowserApp :: String -> [String] -> NamedAction
promptBrowserApp prog args = (inputPromptWithHistCompl xpConfig "browser-app" ?+ go) ? "Prompt (browser app)"
  where go url = spawn $ sdRun prog url $ program prog (args ++ ["--app=" <> url])

inputPromptProg :: XP.XPConfig -> String -> X (Maybe String)
inputPromptProg xpc name = do
  cmds <- io XP.Shell.getCommands
  let cf = XP.Shell.getShellCompl cmds (XP.searchPredicate xpc)
  XP.Input.inputPromptWithCompl xpc name cf

promptSpawnOnByWindowPPID :: X ()
promptSpawnOnByWindowPPID = do
  mPID <- getAlt <$> withFocii (\_ w -> Alt <$> runQuery pid w)
  ps   <- lines <$> readProcess "ps" ["-o","pid,args","hf"]

  let ps' | Just cPID <- mPID = [psLn ++ curStr | psLn <- ps, let curStr = if (show (toInteger cPID) ++ " ") `L.isInfixOf` psLn then " (FOCUSED)" else ""]
          | otherwise         = ps

  XP.Input.inputPromptWithCompl xpcPID "PID" (cfPID ps') ?+ goPIDInput
  where
    xpc    = xpConfig
    xpcPID = xpConfig { XP.sorter = flip const }

    cfPID ps s =
      let predicate = XP.searchPredicate xpcPID s
          predicate' = L.isInfixOf " \\_"

          go back (x:xs)
            | predicate x = case span predicate' xs of
                              (ys,zs) -> reverse (go' (x:back)) ++ [x] ++ ys ++ go [] zs
            | otherwise   = go (x:back) xs
          go _    []  = []

          go' xs = case span predicate' xs of
                     (_:ys,zs) -> ys ++ take 1 zs
                     ([],_:zs) -> zs

       in return $ go [] ps

    goPIDInput  s    = case fromIntegral @Int . read . takeWhile isNumber . dropWhile (not.isNumber) $ s of { !p -> promptTo p ?+ doSetManage p }
    promptTo    p    = wsPrompt' $ printf "(PPID=%i) Shift to tag: " (toInteger p)
    doSetManage p ws = SpawnOn.manageByPPID p $ if null ws then idHook else doShift ws

-- * Programs

dialog :: Terminal
dialog = tName "dialog" <> tSize "130x40"

-- | Attach tmux session (or create one) in a new terminal window.
tmux :: Maybe String -> X ()
tmux msession = spawn $
  inTerm (maybe mempty (tName . ("tmux-"++)) msession <> topt "-sl" (Just "0")) $
    program "tmux" ("attach-session" : maybe [] (("-t":).pure) msession)

-- * Layout modifiers

data MyAmbiguity = MyAmbiguity deriving (Read, Show)

instance NoBorders.SetsAmbiguous MyAmbiguity where
  hiddens (MyAmbiguity{}) wset lr _mst wrs = tiled wrs `L.union` floats
    where
      tiled [(w,_)] = [w]
      tiled xs      = [w | (w,r) <- xs, r == lr]
      floats =
        [w |
          W.Screen{workspace=wspace, screenDetail=SD sr} <- W.screens wset
        , w  <- W.integrate' $ W.stack wspace
        , wr <- maybeToList $ M.lookup w (W.floating wset)
        , sr == scaleRationalRect sr wr
        ]

-- * Hint transformer

data HINT = HINT deriving (Eq, Show, Read)

instance MultiToggle.Transformer HINT Window where
  transform HINT x k = k
    (Layout.Renamed.renamed [Layout.Renamed.PrependWords "Hint"] $ LayoutHints.layoutHintsToCenter x)
    (\(ModifiedLayout _ (ModifiedLayout _ l)) -> l)

-- * Debug

data DebugXS = DebugXS { debugIgnoreProps :: !(Atom -> Bool) }

instance ExtensionClass DebugXS where
  initialValue = DebugXS (const True)

debugEventHook :: Event -> X All
debugEventHook e = do
  ignoreProp <- XS.gets debugIgnoreProps
  case e of
    PropertyEvent{..} | not (ignoreProp ev_atom) -> DebugEvents.debugEventsHook e
    _ -> return (All True)

debugWindowSet :: _
debugWindowSet = DebugStack.debugStackString >>= \str ->
  trace ("print debug: \n" <> str) >>= \_ -> Notify.notifyLastS str

data HiddenW = HiddenW { getHW :: Maybe Window }

instance ExtensionClass HiddenW where
  initialValue = HiddenW Nothing

showHideW = XS.gets getHW >>= \win -> case win of
  Just w  -> maximizeWindowAndFocus w >>= \_ -> XS.put (HiddenW Nothing)
  Nothing -> withFocused $ \w -> XS.put (HiddenW (Just w)) >>= \_ -> minimizeWindow w

button8 :: Button
button8 = 8

-- | Like (?+) but generalized for MonadPlus.
(?++) :: MonadPlus m => m (Maybe a) -> (a -> m b) -> m b
(?++) x f = x >>= \r -> case r of {Just q->f q;_->mzero}

