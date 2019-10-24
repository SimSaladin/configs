{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

------------------------------------------------------------------------------
-- |
-- Module      : MyLayouts
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@relexsolutions.com>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module MyLayouts
  ( myLayout
  -- * WindowNavigation
  , Navigate(..)
  -- * Manage docks
  , docks
  , ToggleStruts(ToggleStruts)
  -- * Borders & spacing
  , toggleBorder
  , toggleScreenSpacingEnabled
  , toggleWindowSpacingEnabled
  -- * Hinting
  , hintsEventHook
  -- * Modifiers (toggle)
  , Toggle(Toggle)
  , REFLECTX(REFLECTX)
  , REFLECTY(REFLECTY)
  , StdTransformers(MIRROR, NBFULL, NOBORDERS)
  , HINT(HINT)
  -- * Minimize & maximize
  , isHidden
  , minimizeEventHook
  , maximizeRestoreTiled
  , MagnifyMsg
  -- * Floats
  , centerOnScreen
  , floatNextHook
  , toggleFloatAllNew
  , positionStoreEventHook
  , positionStoreManageHook
  -- * Urgents
  , focusUrgent
  , BorderUrgencyHook(..)
  ) where

import           Prelude hiding ((>>))
import qualified Data.Map      as M
import           Data.Maybe    (maybeToList)
import           Data.Ratio    ((%))
import qualified Control.Monad ((>>))

import           XMonad
import qualified XMonad.StackSet as W

import           XMonad.Actions.FloatKeys            (keysMoveWindowTo)
import           XMonad.Actions.NoBorders            (toggleBorder)
import           XMonad.Config.Prime                 (Prime, addLayout, modifyLayout, resetLayout)
import qualified XMonad.Config.Prime                 as Prime
import           XMonad.Hooks.FloatNext              (floatNextHook, toggleFloatAllNew, willFloatNext, willFloatAllNew)
import           XMonad.Hooks.ManageDocks            (ToggleStruts(..), avoidStruts, docks)
import           XMonad.Hooks.Minimize               (minimizeEventHook)
import           XMonad.Hooks.PositionStoreHooks     (positionStoreEventHook, positionStoreManageHook)
import           XMonad.Hooks.UrgencyHook            (BorderUrgencyHook(..), focusUrgent)
import           XMonad.Hooks.ManageHelpers          (isInProperty)
import           XMonad.Layout.BinarySpacePartition  (emptyBSP)
import           XMonad.Layout.BoringWindows         (boringWindows)
import           XMonad.Layout.GridVariants          (Grid(Grid))
import           XMonad.Layout.LayoutHints           (hintsEventHook, layoutHintsToCenter)
import           XMonad.Layout.LayoutModifier        (ModifiedLayout(..))
import           XMonad.Layout.Magnifier             (MagnifyMsg, magnifierOff)
import           XMonad.Layout.Maximize              (maximizeRestore, maximizeWithPadding)
import           XMonad.Layout.Minimize              (minimize)
import           XMonad.Layout.MultiToggle           (Toggle(..), Transformer(..), mkToggle1)
import           XMonad.Layout.MultiToggle.Instances (StdTransformers(MIRROR, NBFULL, NOBORDERS))
import           XMonad.Layout.NoBorders             (SetsAmbiguous(..), lessBorders)
import           XMonad.Layout.OneBig                (OneBig(OneBig))
--import           XMonad.Layout.PositionStoreFloat    (positionStoreFloat)
import           XMonad.Layout.Reflect               (REFLECTX(..), REFLECTY(..))
import           XMonad.Layout.Renamed               (Rename(..), renamed)
import           XMonad.Layout.Spacing               (Border(Border), spacingRaw, toggleScreenSpacingEnabled, toggleWindowSpacingEnabled)
import           XMonad.Layout.ThreeColumns          (ThreeCol(ThreeColMid))
import           XMonad.Layout.WindowNavigation      (Navigate(Go, Swap, Move, Apply), configurableNavigation, navigateColor)
import           XMonad.Util.PureX                   (curScreen)

import           MyTheme

maximizeRestoreTiled :: Window -> X ()
maximizeRestoreTiled w = modifyWindowSet (W.sink w) >> sendMessage (maximizeRestore w)
  where (>>) = (Control.Monad.>>)

centerOnScreen :: Window -> X ()
centerOnScreen w = curScreen >>= \screen ->
  let sr  = screenRect (W.screenDetail screen)
      pos = (rect_x sr + round (rect_width sr % 2), rect_y sr + round (rect_height sr % 2))
   in keysMoveWindowTo pos (0.5, 0.5) w

willFloat :: Query Bool
willFloat = liftX (willFloatNext <||> willFloatAllNew)

-- | Looks for @_NET_WM_STATE_HIDDEN@.
isHidden :: Query Bool
isHidden  = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_HIDDEN"

myLayout :: LayoutClass l Window => Prime l _
myLayout = do
  -- NOTE: resetLayout sets the head, addLayout adds to the right of (|||)
  resetLayout emptyBSP
  addLayout (ThreeColMid 1 (1/30) (4/9))
  addLayout (Tall 1 (3/100) (1/2))
  addLayout (Grid (16/9))
  addLayout (OneBig (2/3) (2/3))
  -- addLayout positionStoreFloat

  -- NOTE: MIRROR with REFLECTX/Y is most intuitive when mirror goes first.
  modifyLayout (mkToggle1 MIRROR)
  modifyLayout (mkToggle1 REFLECTX)
  modifyLayout (mkToggle1 REFLECTY)
  modifyLayout (mkToggle1 NOBORDERS)
  modifyLayout (mkToggle1 HINT)

  -- NOTE: WindowNavigation interacts badly with some modifiers like "maximize" and "spacing", apply those after it.
  modifyLayout (configurableNavigation $ navigateColor colBase00)
  -- border/spacing/maximize/magnify
  modifyLayout (mySpacing 15 5)
  modifyLayout (lessBorders MyNoBorders)
  modifyLayout (maximizeWithPadding 80)
  modifyLayout magnifierOff
  -- NOTE: Apply avoidStruts late so that other modifiers aren't affected.
  modifyLayout avoidStruts
  -- NOTE: This replaces the layout, including modifiers applied before it.
  modifyLayout (mkToggle1 NBFULL)
  -- minimize/boring
  modifyLayout boringWindows
  modifyLayout minimize
  where
    (>>) = (Prime.>>)

mySpacing :: _ => Integer -> Integer -> l Window -> ModifiedLayout _ _ Window
mySpacing sd wd = spacingRaw True (f sd) True (f wd) True
  where
    f n = Border n n n n

-- | Remove borders on a tiled window covering the whole screen *on per screen basis*.
data MyNoBorders = MyNoBorders deriving (Read, Show)

instance SetsAmbiguous MyNoBorders where
  hiddens MyNoBorders wset _lr _mst wrs =
    [ w | [(w,_)] <- [wrs] ] ++
    [ w -- floating
      | s  <- W.screens wset
      , w  <- W.integrate' $ W.stack $ W.workspace s
      , wr <- maybeToList $ M.lookup w (W.floating wset)
      , sr <- [screenRect (W.screenDetail s)]
      , sr == scaleRationalRect sr wr ]

data HINT = HINT
  deriving (Eq, Show, Read)

instance Transformer HINT Window where
  transform HINT x k = k (renamed [PrependWords "Hint"] $ layoutHintsToCenter x) (\(ModifiedLayout _ (ModifiedLayout _ l)) -> l)
