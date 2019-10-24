
------------------------------------------------------------------------------
-- |
-- Module      : MyMain
-- Description :
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@relexsolutions.com>
-- Stability   : unstable
-- Portability : unportable
--
-- Start/stop/restart customization.
--
------------------------------------------------------------------------------

module MyMain where

import qualified MyNotify
import           MyRun
import qualified MyXmobar
import           Prelude
import           XMonad

myProg :: FilePath
myProg = "xmonad-my"

myRestart :: X ()
myRestart = do
  logInfo "Restart initiated"
  MyNotify.exitHook
  MyXmobar.exitHook
  restart myProg True

myLaunch :: (LayoutClass l' Window, Read (l' Window))
         => (XConfig (Choose Tall (Choose (Mirror Tall) Full)) -> IO (XConfig l')) -> IO ()
myLaunch conf = conf def >>= launch
