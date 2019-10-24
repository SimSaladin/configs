
------------------------------------------------------------------------------
-- |
-- Module      : Util
-- Description : Short description
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Misc. utilities to go with XMonad configs.
--
------------------------------------------------------------------------------

module Util where

import           Prelude

import qualified Control.Exception as E
import qualified Data.List
import           Data.Foldable     (toList)
import qualified System.IO         as IO

import           XMonad
import qualified XMonad.Config.Prime        as Prime
import qualified XMonad.Hooks.UrgencyHook   as Urgency

-- * Logging

logInfo, logError :: MonadIO m => String -> m ()
logInfo  = io . E.handle @IOError logErrorShow    . IO.hPutStrLn IO.stdout
logError = io . E.handle @IOError (\_ -> pure ()) . IO.hPutStrLn IO.stderr
{-# INLINE logInfo #-}
{-# INLINE logError #-}

logErrorShow :: (MonadIO m, Show s) => s -> m ()
logErrorShow = logError . show
{-# INLINE logErrorShow #-}

-- * XMonad.Config.Prime

urgencyHook :: (Urgency.UrgencyHook h, LayoutClass l Window) => h -> Prime.Prime l l
urgencyHook = Prime.apply . flip Urgency.withUrgencyHookC Urgency.urgencyConfig { Urgency.suppressWhen = Urgency.Focused }

-- * Misc.

-- "Data.List.intersperse" generalized for any "Foldable" of some "Monoid".
sepByConcat :: (Monoid a, Foldable t) => a -> t a -> a
sepByConcat a = mconcat . Data.List.intersperse a . toList
