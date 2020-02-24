{-# LANGUAGE TypeApplications #-}

------------------------------------------------------------------------------
-- |
-- Module      : MyXmobar
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson
-- Stability   : unstable
-- Portability : unportable
--
-- Xmobar configuration.
--
------------------------------------------------------------------------------

module MyXmobar
  ( myStatusBars
  , exitHook
  ) where

import qualified Control.Exception             as E
import           Control.Monad
import           Data.IORef
import           Graphics.X11.Xinerama         (getScreenInfo)
import           Prelude
import qualified System.IO                     as IO
import           System.IO.Unsafe              (unsafePerformIO)
import qualified System.Posix                  as Posix
import           XMonad                        hiding (spawn)
import qualified XMonad.Config.Prime           as Prime
import qualified XMonad.Hooks.DynamicBars      as DBar

import           MyRun
import           MyTheme
import qualified Xmobar                        as XB

myStatusBars :: Prime.Prime l l
myStatusBars =
  (Prime.logHook         Prime.=+ DBar.multiPPFormat runPP focusedPP unfocusedPP) Prime.>>
  (Prime.startupHook     Prime.=+ DBar.dynStatusBarStartup'   (dynStatusBar) (partialCleanup)) Prime.>>
  (Prime.handleEventHook Prime.=+ DBar.dynStatusBarEventHook' (dynStatusBar) (partialCleanup))

exitHook :: X ()
exitHook = io cleanup

sbarHackRef :: IORef [(ScreenId, (Handle, ProcessID))]
sbarHackRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE sbarHackRef #-}

dynStatusBar :: DBar.DynamicStatusBar -- ~ ScreenId -> IO Handle
dynStatusBar screen@(S sid) = do
  screenInfo <- E.bracket (openDisplay "") closeDisplay getScreenInfo
  if check screenInfo then go else IO.openFile "/dev/null" IO.WriteMode
    where
      check srs
        | r:rs <- drop sid srs = not $ any (r `containedIn`) (take sid srs ++ rs)
        | otherwise            = False
      go = do
        r <- spawnPipeIO $ XB.xmobar xmobarConfig
        atomicModifyIORef sbarHackRef $ \xs -> ((screen, r) : xs, ())
        return (fst r)

      xmobarConfig =
        xmobarConfig_ { XB.position = XB.OnScreen sid XB.Top }

cleanup :: IO ()
cleanup = readIORef sbarHackRef >>= mapM_ (destroy . snd)

partialCleanup :: ScreenId -> IO ()
partialCleanup sid = readIORef sbarHackRef >>= mapM_ destroy . lookup sid

destroy (_,pId) = do
  E.try @E.SomeException (Posix.signalProcess Posix.sigTERM pId) -- TODO SomeException
  E.try @E.SomeException (Posix.getProcessStatus True False pId)
