{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Prompt.Qutebrowser
-- Description : qutebrowser
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module XMonad.Prompt.Qutebrowser where

import qualified Control.Exception
import           Data.Functor
import qualified Data.List           as List
import           Prelude
import qualified System.Directory
import           XMonad              (X, io, trace)
import qualified XMonad.Prompt       as XP
import qualified XMonad.Prompt.Input as XP.Input

import           MyRun

-- | --qt-arg name app_name etc. https://peter.sh/experiments/chromium-command-line-switches/
-- $XDG_RUNTIME_DIR/qutebrowser/$session/runtime/ipc-*
-- /usr/share/qutebrowser/scripts/open_url_in_instance.sh
qutebrowser :: String -> X ()
qutebrowser "" = qutebrowser "default"
qutebrowser p  = spawn $ sdRun "qutebrowser" p ("qutebrowser", ["-r", p])

qutebrowserP :: _ -> String -> X (Maybe String)
qutebrowserP xpc nm = io qutebrowserCompl >>= XP.Input.inputPromptWithCompl xpc nm

qutebrowserCompl :: IO (String -> IO [String])
qutebrowserCompl =
  System.Directory.getXdgDirectory System.Directory.XdgData "qutebrowser" >>=
    Control.Exception.try . System.Directory.listDirectory >>=
      either @IOError (\e -> trace (show e) $> []) (pure . f) >>=
        pure . XP.mkComplFunFromList'
  where
    f = filter $ \x -> not
      $ ("-qutebrowser" `List.isSuffixOf` x)
      || ("." `List.isPrefixOf` x)
      || (x `elem` ["null", "userscripts", "qtwebengine_dictionaries", "blocked-hosts"])
