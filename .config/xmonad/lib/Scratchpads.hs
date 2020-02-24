{-# LANGUAGE LambdaCase #-}
------------------------------------------------------------------------------
-- |
-- Module      : Scratchpads
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module Scratchpads
  ( Scratchpad,
  mkXScratchpads,
  scratchpadsManageHook,
  scratchpadCompl,
  toggleScratchpad,
  minimizeScratchpads,

  ) where

import           Control.Monad
import qualified Data.List                  as L
import           Prelude
import           XMonad
import           XMonad.Actions.Minimize    (maximizeWindowAndFocus, minimizeWindow)
import           XMonad.Hooks.ManageHelpers
import qualified XMonad.Prompt              as XP
import qualified XMonad.StackSet            as W
import Data.Semigroup

currentWindows :: W.StackSet i l a sid sd -> [a]
currentWindows = W.integrate' . W.stack . W.workspace . W.current

-- | Custom implementation: affected by NamedScratchpads and ExclusiveScratchpads.
data Scratchpad = SP
  { spName      :: String
  , spCmd       :: X ()
  , spQuery     :: Query Bool
  , spHook      :: ManageHook
  , spExclusive :: [String]
  }

mkXScratchpads :: [(String, X (), Query Bool)] -> ManageHook -> [Scratchpad]
mkXScratchpads inp hook = res
  where
    res = [SP name cmd query hook other | (name, cmd, query) <- inp, let other = L.delete name (map spName res)]

scratchpadsManageHook :: [Scratchpad] -> [MaybeManageHook]
scratchpadsManageHook xs = [spQuery sp -?> spHook sp | sp <- xs]

-- scratchpadCompl :: [Scratchpad] -> XP.ComplFunction
scratchpadCompl xpc pads = XP.mkComplFunFromList' xpc (map spName pads)

scratchpadsLookup :: [Scratchpad] -> Query (Maybe Scratchpad)
scratchpadsLookup (x:xs) = spQuery x >>= \r -> if r then return (Just x) else scratchpadsLookup xs
scratchpadsLookup []     = return Nothing

-- | First, minimize any pads exclusive with target.
-- Then, look for an existing instance in focused workspace.
-- If that fails, look for an instance across all windows.
toggleScratchpad :: [Scratchpad] -> String -> X ()
toggleScratchpad spads name = whenJust (L.find ((name ==) . spName) spads) $ \sp -> do
  minimizeScratchpads [x | x <- spads, spName x `elem` spExclusive sp]
  withWindowSet (filterM (runQuery (spQuery sp)) . currentWindows) >>= \case
    w : _ -> toggleWindow Nothing (spHook sp) w
    []    -> withWindowSet (filterM (runQuery (spQuery sp)) . W.allWindows) >>= \case
        w : _ -> toggleWindow Nothing (spHook sp) w
        []    -> spCmd sp

minimizeScratchpads :: [Scratchpad] -> X ()
minimizeScratchpads xs = withWindowSet $
  mapM_ (\w -> runQuery (scratchpadsLookup xs) w >>= maybe mempty (go w)) . currentWindows
  where
    go w sp = toggleWindow (Just False) (spHook sp) w


-- | Toggle minimize/maximize of a window. See "XMonad.Actions.Minimize"
-- First argument: only maximize (True) or minimize (False).
toggleWindow :: Maybe Bool -> ManageHook -> Window -> X ()
toggleWindow ma mh w = do
  f <- appEndo <$> runQuery mh w
  liftM2 (,) (runQuery isHidden w) inCurrentWS >>= \case
    (True, True)   | ma /= Just False -> modifyWindowSet (f . W.insertUp w . W.delete' w) >> maximizeWindowAndFocus w
    (True, False)  | ma /= Just False -> modifyWindowSet (W.currentTag >>= flip W.shiftWin w) >> windows f >> maximizeWindowAndFocus w
    (False, False) | ma /= Just False -> modifyWindowSet (W.currentTag >>= flip W.shiftWin w) >> windows f
    (False, True)  | ma /= Just True  -> minimizeWindow w >> windows (W.peek >>= \w' -> maybe id W.focusWindow w' . W.shiftMaster . W.focusWindow w)
        -- Above: Minimize & shift to master, so that we won't bump into the minimized window when refocusing after dead window.
    _                                 -> return ()
  where
    inCurrentWS = withWindowSet (return . elem w . currentWindows)
    isHidden = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_HIDDEN"
