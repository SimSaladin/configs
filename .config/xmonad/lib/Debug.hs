
------------------------------------------------------------------------------
-- |
-- Module      : Debug
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module Debug where

import           Data.Monoid
import qualified MyNotify                    as Notify
import           Prelude
import           XMonad
import qualified XMonad.Hooks.DebugEvents    as H (debugEventsHook)
import qualified XMonad.Hooks.DebugStack     as H (debugStackString)
import qualified XMonad.Util.ExtensibleState as XS

data DebugXS = DebugXS { debugIgnoreProps :: !(Atom -> Bool) }

instance ExtensionClass DebugXS where
  initialValue = DebugXS (const True)

debugEventHook :: Event -> X All
debugEventHook e = do
  ignoreProp <- XS.gets debugIgnoreProps
  case e of
    PropertyEvent{..}
      | not (ignoreProp ev_atom) -> H.debugEventsHook e
    _ -> return (All True)

debugWindowSet = Notify.notifyLastS =<< H.debugStackString
