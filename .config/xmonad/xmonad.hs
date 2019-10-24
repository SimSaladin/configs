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
------------------------------------------------------------------------------

module Main (main) where

import           Control.Monad                      ((>=>))
import qualified Control.Monad

import qualified XMonad.StackSet                    as W

import qualified XMonad.Actions.CopyWindow          as A (copy, kill1)
import           XMonad.Actions.CycleRecentWS       (cycleWindowSets)
import qualified XMonad.Actions.CycleWS             as CycleWS
--import qualified XMonad.Actions.DynamicWorkspaceGroups as DynWSG
import qualified XMonad.Actions.DynamicWorkspaces   as DynWS
import qualified XMonad.Actions.FlexibleManipulate  as Flex
import qualified XMonad.Actions.FloatSnap           as A (snapGrow, snapMove, snapShrink)
import qualified XMonad.Actions.GridSelect          as GS
import           XMonad.Actions.OnScreen            (Focus(..), onScreen)
import qualified XMonad.Actions.PhysicalScreens     as PScreen
import qualified XMonad.Actions.RotSlaves           as RotSlaves
import qualified XMonad.Actions.UpdatePointer       as A (updatePointer)
import qualified XMonad.Actions.WorkspaceNames      as WSNames

import           XMonad.Config.Prime                hiding (spawn, (>>))
import qualified XMonad.Config.Prime                as Arr ((>>))

import qualified XMonad.Hooks.EwmhDesktops          as EWMH
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.Hooks.Place                 as Place (placeHook, smart, withGaps)

import qualified XMonad.Layout.BinarySpacePartition as BSP
import qualified XMonad.Layout.BoringWindows        as BW
import qualified XMonad.Layout.Magnifier            as Magnifier

import qualified XMonad.Prompt.AppLauncher          as XP (launchApp)
import qualified XMonad.Prompt.Directory            as XP (directoryPrompt)
import           XMonad.Prompt.Input                (inputPrompt, inputPromptWithCompl, (?+))
import qualified XMonad.Prompt.Pass                 as XP (passPrompt)
import qualified XMonad.Prompt.Window               as XP (WindowPrompt(Goto), allWindows, windowPrompt)

import           XMonad.Util.PureX
import           XMonad.Util.Types                  (Direction1D(..), Direction2D(..))
import           XMonad.Util.Ungrab                 (unGrab)

import           Commands
import           Debug
import           MyLayouts
import           MyMain
import qualified MyNotify    as Notify
import           MyRun
import           MyTheme
import qualified MyXmobar
import           Scratchpads
import           Util

main :: IO ()
main = myLaunch configPrime

configPrime :: _ => Prime l _
configPrime = do
  terminal           =: "urxvtc"
  borderWidth        =: 1
  focusedBorderColor =: colCyan
  normalBorderColor  =: colBase02
  clientMask         =+ focusChangeMask -- default: structureNotifyMask .|. enterWindowMask .|. propertyChangeMask@
  -- rootMask -- default: substructureRedirectMask .|. substructureNotifyMask .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask .|. buttonPressMask
  modMask            =: mod4Mask
  mouseBindings      =+ myMouseBindings
  focusFollowsMouse  =: True
  clickJustFocuses   =: False
  startupHook        =+ Notify.startupHook
  startupHook        =+ EWMH.ewmhDesktopsStartup   -- Initialize and advertise EWMH support
  logHook            =+ myUpdatePointer (0.5, 0.5) (0.4, 0.4)
  logHook            =+ EWMH.ewmhDesktopsLogHookCustom id
  handleEventHook    =+ debugEventHook
  handleEventHook    =+ positionStoreEventHook
  handleEventHook    =+ minimizeEventHook    -- Handle minimize/maximize requests
  handleEventHook    =+ hintsEventHook       -- Refreshes the layout whenever a window changes its hints.
  handleEventHook    =+ EWMH.ewmhDesktopsEventHook -- Intercepts _NET_CURRENT_DESKTOP, _NET_WM_DESKTOP, _NET_ACTIVE_WINDOW
  handleEventHook    =+ EWMH.fullscreenEventHook
  --handleEventHook    =+ EWMHHack.fullscreenEventHook
  manageHook         =+ positionStoreManageHook Nothing
  manageHook         =+ myManageHook
  MyXmobar.myStatusBars
  apply docks -- Includes: manageDocks, docksStartupHook, docksEventHook
  urgencyHook (BorderUrgencyHook colGreen)
  urgencyHook Notify.urgencyHook
  myLayout
  apply $ addKeys' keysConfig (cmdsKeys myCmds)
  where
    (>>) = (Arr.>>)

myManageHook :: ManageHook
myManageHook = composeOne $ scratchpadsManageHook myScratchpads ++
  [ transience
  , className =? "Xmessage" -?> doCenterFloat -- program "xmessage"
  , className =? "Display"  -?> doCenterFloat
  , className =? "Xmag"     -?> doCenterFloat -- program "xmag"
  , appName   =? "dialog"   -?> doRectFloat (W.RationalRect 0.25 0.15 0.5 0.5) -- program "urxvtc -name dialog"
  , isDialog                -?> doCenterFloat
  , Just <$> defHook
  ]
  where
    defHook = Place.placeHook (Place.withGaps (30, 30, 30, 30) $ Place.smart (0.5, 0.5)) <+> floatNextHook
    -- default: insertPosition Above Newer

myScratchpads :: [Scratchpad]
myScratchpads = mkXScratchpads
  [ ("pad0",    tmux   def{ windowName = "pad0" }        "pad0",       appName =? "pad0")
  , ("ncmpcpp", spawnT def{ windowName = "pad-ncmpcpp" } "ncmpcpp" [], appName =? "pad-ncmpcpp")
  ] (doRectFloat (W.RationalRect 0.2 0.1 0.6 0.6))

-- Modified to not fire on spammy property updates (e.g. status bar stuff).
myUpdatePointer :: _ -> _ -> X ()
myUpdatePointer x y = whenX (check <$> asks currentEvent) (A.updatePointer x y)
  where check (Just PropertyEvent{}) = False
        check _                      = True

-- | Mouse button actions.
--
-- Kensington button keycodes:
--  bottom-left:  button1 ("left click")
--  bottom-right: button3 ("right click")
--  top-right:    button8 ("back")
--  top-left:     button2 ("middle click")
--  scroll wheel: button4
--                button5
myMouseBindings :: [((ButtonMask, Button), Window -> X ())]
myMouseBindings =
  [ ((modm,               button1), \w -> Flex.mouseWindow Flex.discrete w)
  , ((modm .|. shiftMask, button1), \w -> Flex.mouseWindow Flex.resize w)
  , ((modm,               button2), \w -> windows $ W.focusWindow w >> W.swapMaster)
  , ((controlMask,        button3), \_ -> mpv_clipboard)
  , ((controlMask,        button8), \_ -> mpv_clipboard)
  ]
  where
    (>>) = (Control.Monad.>>)
    modm = mod4Mask

    button8 :: Button
    button8 = 8

keysConfig :: KeysConfig
keysConfig = def
  { showKeys = \t xs -> Notify.notify_ $ Notify.summary t $ Notify.body ("<tt>" ++ unlines xs ++ "</tt>") def
  }

myCmds :: Cmd ()
myCmds =
  cmdGroup "XMonad & X11"
    (  ak "M-q"         "restart" myRestart
    <> ak "M-S-q"       "exit"    (io exitSuccess)
    <> ak "M-$"         "lock"    (spawn "physlock" [])
    <> ak "M-<Esc>"     "debug-windowset"  debugWindowSet
    )

  <> cmdGroup "Window"
    (  ak "M-S-c"       "kill-window-copy" A.kill1
    <> ak "M-g f"       "Go to window (XP)"               (XP.windowPrompt xpConfigAuto XP.Goto XP.allWindows)
    <> ak "M-g s"       "Go to window (GS)"               (GS.goToSelected gsconfig1)
    <> ak "M-g S-n"     "Move window to new tag (XP)"     (inputPrompt xpConfig "New workspace for window" ?+ \s -> DynWS.addHiddenWorkspace s >> (windows $ W.shift s))
    <> ak "M-g c"       "Copy window to this tag (XP)"    (WSNames.workspaceNamePrompt xpConfigAuto (windows . A.copy))
    <> ak "M-g m"       "Move window to this tag (XP)"    (WSNames.workspaceNamePrompt xpConfigAuto (windows . W.shift))
    <> ak "M-f m"   "Swap Master"         (windows W.swapMaster) -- TODO needs boring
    <> ak "M-f n"   "Swap Up"             (windows W.swapUp)
    <> ak "M-f p"   "Swap Down"           (windows W.swapDown)
    <> ak "M-f u"   "Focus Urgent"        focusUrgent
    <> ak "M-f ."   "Rotate Slaves Up"    RotSlaves.rotSlavesUp
    <> ak "M-f ,"   "Rotate Slaves Down"  RotSlaves.rotSlavesDown
    <> ak "M-f M-." "Rotate Up"           RotSlaves.rotAllUp
    <> ak "M-f M-," "Rotate Down"         RotSlaves.rotAllDown
    <> ak "M-f M-m" "Focus Master"        BW.focusMaster
    <> ak "M-f M-n" "Focus Up"            BW.focusUp
    <> ak "M-f M-p" "Focus Down"          BW.focusDown
    <> ck "M-b M-b" ("toggle-current-border", withFocused toggleBorder >> refresh)
    <> cmdGroup "Window Navigation"
      (  keysOf' "M-"   directions2D Go
      <> keysOf' "M-S-" directions2D Swap
      )
    <> cmdGroup "Floating"
      (  keysOf "M-f "    directions2D "Move float"   (withFocused . flip A.snapMove   Nothing)
      <> keysOf "M-f S-"  directions2D "Grow float"   (withFocused . flip A.snapGrow   Nothing)
      <> keysOf "M-f C-"  directions2D "Shrink float" (withFocused . flip A.snapShrink Nothing)
      <> ak "M-f c"   "Center"              (withFocused centerOnScreen)
      <> ak "M-f s"   "Sink"                (withFocused (windows . W.sink))
      )
    )

  <> cmdGroup "Screens"
    (  ak "M-C-<Right>" "View screen (Prev)" CycleWS.prevScreen
    <> ak "M-C-<Left>"  "View screen (Next)" CycleWS.nextScreen
    <> keysOf "M-S-"  skeys "Send to screen"      (PScreen.sendToScreen def)
    <> keysOf "M-"    skeys "View screen"         (PScreen.viewScreen def)
    <> keysOf "M-M1-" skeys "View screen here"    (withScreen $ \s -> windows $ W.currentTag >>= \t -> onScreen (W.greedyView t) FocusCurrent s)
    <> keysOf "M-C-"  skeys "View this on screen" (withScreen $ \s -> windows $ W.currentTag >>= \t -> onScreen (W.greedyView t) FocusNew s)
    )
  <> cmdGroup "Workspaces"
    (  keysOf "M-; "   tags "View tag"            (DynWS.withNthWorkspace W.greedyView)
    <> keysOf "M-; M-" tags "Copy focused to tag" (DynWS.withNthWorkspace A.copy)
    <> keysOf "M-S-; " tags "Move focused to tag" (DynWS.withNthWorkspace W.shift)
    <> ak "M-y"         "Cycle (focus) recent tags" (cycleRecentHiddenWS [xK_Super_L, xK_Alt_L] xK_y xK_p)
    <> ak "M-S-n"       "Shift current tag forward"  (WSNames.swapTo Next)
    <> ak "M-S-p"       "Shift current tag backward" (WSNames.swapTo Prev)
    <> ak "M-g g"       "Go to tag (XP)"                  (WSNames.workspaceNamePrompt xpConfig (windows . W.greedyView))
    <> ak "M-g n"       "New tag (XP)"                    (DynWS.addWorkspacePrompt xpConfig)
    <> ak "M-g d"       "Remove this tag (if empty)"      (removeNoVisibleWS)
    <> ak "M-g r"       "Rename this tag (XP)"            (WSNames.renameWorkspace xpConfig)
    )

  <> cmdGroup "Layout"
    (  ck "M-<Space>"   NextLayout
    <> ck "M-S-<Space>" FirstLayout
    <> ck "M-x"         Shrink
    <> ck "M-S-x"       Expand
    <> ck "M-."         (IncMasterN 1)
    <> ck "M-,"         (IncMasterN (-1))
    <> ak "M-m"         "toggle-maximized" (withFocused maximizeRestoreTiled)
    <> cmdGroup "BSP"
      (  ck "M-b M-y" BSP.SelectNode
      <> ck "M-b M-p" BSP.MoveNode
      <> ck "M-b M-u" BSP.FocusParent
      <> keysOf' "M-C-" directions2D BSP.ExpandTowards
      )
    )

  <> cmdGroup "Modifiers"
    (  ak "M-b f" "toggle-float-new" toggleFloatAllNew
    <> ak "M-b s" "toggle-spacing" (toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled)
    <> ck "M-b t" ToggleStruts
    <> ck "M-b l" Magnifier.Toggle
    <> ck "M-b b" (Toggle NOBORDERS)
    <> ck "M-b h" (Toggle HINT)
    <> ck "M-b f" (Toggle NBFULL)
    <> ck "M-b m" (Toggle MIRROR)
    <> ck "M-b x" (Toggle REFLECTX)
    <> ck "M-b y" (Toggle REFLECTY)
    <> ck "M-b M-x" (ToggleStruts :>> Toggle NOBORDERS :>> ("Toggle spacing", toggleScreenSpacingEnabled >> toggleWindowSpacingEnabled))
    )


  <> cmdGroup "Process"
    (  ak "M-<Return>"  "new-terminal"     (spawnT_ def)
    <> ak "M-r <Return>" "prompt-run"          (inputPromptProg xpConfig "spawnS"  ?+ spawnS)
    <> ak "M-r r"        "prompt-service"      (inputPromptProg xpConfig "service" ?+ \xs -> service Nothing Nothing (head $ words xs) (tail $ words xs) )
    <> ak "M-r M-r"      "prompt-run-terminal" (inputPromptProg xpConfig "spawnTS" ?+ spawnTS (terminalHold def) )
    <> ak "M-!"          "pad-0"               (toggleScratchpad myScratchpads "pad0")
    <> ak "M-#"          "pad-ncmpcpp"         (toggleScratchpad myScratchpads "ncmpcpp")
    <> ak "M-r v" "mpv-clipboard"          (mpv_clipboard)
    <> ak "M-r b" "bluetoothctl"           (spawnT dialog "bluetoothctl" [])
    <> ak "M-r c" "clipmenu"               (spawn "clipmenu" [])
    <> ak "M-r m" "xmag"                   (unGrab >> spawn "xmag" ["-mag", "2", "-source", "960x540"])
    <> ak "M-r g" "prompt-gimp"            (XP.launchApp xpConfig "gimp")
    <> ak "M-r t" "prompt-tmux(p)"         (tmuxPrompt xpConfig)
    <> ak "M-r s" "prompt-pad"             (inputPromptWithCompl xpConfig "scratchpad" (scratchpadCompl myScratchpads) ?+ toggleScratchpad myScratchpads)
    <> ak "M-r p" "prompt-Pass"            (XP.passPrompt xpConfig)
    <> ak "M-r d" "prompt-ranger"          (XP.directoryPrompt xpConfig "" $ spawnT dialog "ranger" . pure)
    <> ak "M-r q" "prompt-qutebrowser"     (qutebrowserP xpConfigNoHist "qutebrowser" ?+ qutebrowser)
    <> ak "M-r u" "prompt-chrome-app"      (inputPromptWithHistCompl xpConfig "chrome --app" ?+ chromeApp)
    )

  <> cmdGroup "Media"
    (  ak "M-+"   "vol-up"   (volume 3)
    <> ak "M--"   "vol-down" (volume (-3))
    <> ak "M-c m" "pulsemixer" (spawnT dialog "pulsemixer" [])
    <> ak "M-c n" "mpc-next"   (mpc "next")
    <> ak "M-c p" "mpc-prev"   (mpc "prev")
    <> ak "M-c t" "mpc-toggle" (mpc "toggle")
    <> ak "M-c y" "mpc-single" (mpc "single")
    <> ak "M-c r" "mpc-random" (mpc "random")
    <> ak "<XF86MonBrightnessUp>"   "brightness-up"   (backlight 5)
    <> ak "<XF86MonBrightnessDown>" "brightness-down" (backlight (-5))
    <> ak "<XF86AudioMute>"         "mic-0" (mic "0")
    )
    where
      (>>) = (Control.Monad.>>)
      skeys        = zip screenKeys [PScreen.P 0 ..]
      tags         = zip tagKeys [(0::Int)..]
      tagKeys      = map (:[]) ['a'..'z']
      screenKeys   = map (:[]) "wvz"
      directions2D = map (:[]) "kjlh" `zip` [minBound..maxBound @Direction2D]


-- "View WSG (XP)"   , DynWSG.promptWSGroupView   xpConfigAuto "View WSG: ")
-- "Add WSG (XP)"    , DynWSG.promptWSGroupAdd    xpConfig     "New WSG: ")
-- "Forget WSG (XP)" , DynWSG.promptWSGroupForget xpConfig     "Forget WSG: ")

withScreen :: (ScreenId -> X ()) -> PScreen.PhysicalScreen ->  X ()
withScreen f = PScreen.getScreen def >=> mapM_ f

cycleRecentHiddenWS :: [KeySym] -> KeySym -> KeySym -> X ()
cycleRecentHiddenWS =
  cycleWindowSets $ \wset ->
    [W.view (W.tag ws) wset | ws <- W.hidden wset ++ [W.workspace (W.current wset)]]

removeNoVisibleWS :: X ()
removeNoVisibleWS =
  curWorkspace >>= \ws ->
    whenX (fmap and $ mapM (runQuery isHidden) (W.integrate' $ W.stack ws)) DynWS.removeWorkspace
