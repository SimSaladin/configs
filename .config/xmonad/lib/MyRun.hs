{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances          #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
------------------------------------------------------------------------------
-- |
-- Module      : MyRun
-- Copyright   : (c) 2019 Samuli Thomasson
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : non-portable
--
-- "XMonad.Util.Run"
------------------------------------------------------------------------------

module MyRun (
  -- * Spawn
  RunException(..),
  Run(..),
  Spawn(..),
  HasCmd(..),
  Terminal,
  readProcess,
  readProcessWithInput,
  runProcess_,
  shell,
  program,
  term,
  inTerm,
  exec,
  spawnPipeIO,
  -- * Terminal
  tCWD, tName, tSize, tHold,
  topt,
  -- * Systemd
  sdEscape,
  sdRun,

  -- * Re-exports
  Posix.ProcessID,
  IO.Handle,
  IO.BufferMode(..),
  IO.hClose,
  IO.hPutStrLn,
  IO.hSetBuffering,
  IO.stdout,
  IO.stderr,
  ) where

import           XMonad                   hiding (spawn)
import           XMonad.Util.Run          (seconds)

import           Prelude

import           Control.Applicative
import qualified Control.Exception        as E
import           Control.Monad
import qualified Data.Map                 as M
import           Data.Maybe               (maybeToList)
import qualified System.IO                as IO
import qualified System.Posix             as Posix
import           System.Posix             (ProcessID)
import qualified System.Process           as P
import           System.Timeout           (timeout)
import           Text.Printf              (printf)
import Data.Void

import           Codec.Binary.UTF8.String (encodeString)

-- X.Core.spawn          - void (spawnPID ...)
-- X.Core.spawnPID       - xfork (executeFile ...)
-- X.Core.xfork          - forkProcess w/ signal handler handling
-- X.U.Run.safeSpawn     - exec
-- X.U.Run.safeSpawnProg - exec (no arguments)
-- X.U.Run.unsafeSpawn   - exec /bin/sh -c
-- X.U.Run.spawnPipe     - exec /bin/sh -c (return handle for pipe for stdin)

{-
type family Runnable a where
  Runnable P.CmdSpec = SdRun_ `WithFallback` P.CmdSpec

data WithFallback main fallback = WithFallback main fallback
-}

newtype ShellCmd    = ShellCmd String
newtype ProcSpec    = Proc (FilePath, [String])
newtype SystemdUnit = SystemdUnit String
newtype WithPipe a = WithPipe a

type family SystemdRunInfo a b where
  SystemdRunInfo a (IO.Handle, ProcessID) = WithPipe (SystemdRunInfo a ProcessID)
  SystemdRunInfo (IO ()) ProcessID        = () -- fork exec
  SystemdRunInfo ProcSpec ProcessID       = () -- named
  SystemdRunInfo ShellCmd ProcessID       = SystemdRunInfo ProcSpec ProcessID -- transient
  SystemdRunInfo SystemdUnit b            = () -- SystemdRunNamedUnit

tryRunViaSystemd :: a -> SystemdRunInfo a b -> IO (Maybe b)
tryRunViaSystemd _ _ = undefined

runNormal :: a -> IO b
runNormal _ = undefined

data Run a where
  RunProgram :: FilePath -> [String] -> Run ()
  Shell      :: String -> Run ()

  -- | systemd-run [cmd...]
  SdRun :: { unitName       :: !String -- ^ The foo in foo.service
           , unitScope      :: !Bool
           , unitSlice      :: !(Maybe String)
           , unitInstance   :: !String -- ^ bar in foo@bar.service
           , unitIsTemplate :: !Bool   -- ^ Is template unit
           , unitCmd        :: a
           } -> Run a

  -- | systemd-run ... [terminal] [-e ...]
  Term :: Terminal -> Maybe a -> Run a

-- EXCEPTION

data RunException = RunProcessTimeout deriving (Eq, Show)
instance E.Exception RunException

-- CLASS

class HasCmd f a where
  cmdSpec :: a -> f P.CmdSpec

instance Applicative f => HasCmd f P.CmdSpec            where cmdSpec = pure
instance Applicative f => HasCmd f FilePath             where cmdSpec = pure . (`P.RawCommand` [])
instance Applicative f => HasCmd f (FilePath, [String]) where cmdSpec = pure . uncurry P.RawCommand

class Spawn a where
  spawn :: a

instance (MonadIO m, HasCmd m a        ) => Spawn (a -> m Posix.ProcessID) where spawn = cmdSpec >=> \cs -> logCmdInfo "spawn" cs >> xfork (exec cs)
instance (MonadIO m, HasCmd m a        ) => Spawn (a -> m ())              where spawn = spawn >=> \(_::Posix.ProcessID) -> return ()
instance (MonadIO m, HasCmd m (a,b)    ) => Spawn (a -> b -> m ())         where spawn = curry spawn
instance (MonadIO m, HasCmd m (a,(b,c))) => Spawn (a -> b -> c -> m ())    where spawn = flip $ flip . curry (flip (curry spawn))

-- DATA

-- deriving (Eq, Show, Read)

data Terminal = Terminal
  { terminalExec :: !(Maybe FilePath) -- ^ Use some other terminal than XConfig.terminal
  , terminalOpts :: !(M.Map String (Maybe String))
  } deriving (Eq, Show, Read)

instance Default   Terminal where def    = mempty
instance Monoid    Terminal where mempty = Terminal mempty mempty
instance Semigroup Terminal where a <> b = Terminal { terminalExec = terminalExec a <|> terminalExec b
                                                    , terminalOpts = terminalOpts a <> terminalOpts b }

instance forall m a. (m ~ X, HasCmd m a) => HasCmd m (Run a) where
  cmdSpec (RunProgram a xs) = pure (P.RawCommand a xs)
  cmdSpec (Shell a)         = pure (P.RawCommand "/bin/sh" ["-c", a])

  cmdSpec (Term Terminal{..} mcs) = do
    bin <- maybe (asks (terminal . config)) return terminalExec
    cmd <- maybe (pure []) (cmdSpec >=> \cs -> pure ("-e" : uncurry (:) (showCmdSpec cs))) mcs
    cmdSpec . sdRun "" "" $ program bin (opts <> cmd)
      where
        opts = [x | (o,v) <- M.toList terminalOpts, x <- o : maybeToList v]

  cmdSpec SdRun{..} = do
    cspec <- cmdSpec unitCmd
    -- TODO avoid IO somehow; foreign import systemd-escape instead?
    sdUnit <-
      case unitName of
        ""                 -> pure []
        _ | unitIsTemplate -> sdEscape ["--template", unitName <> "@.service"] [unitInstance]
          | otherwise      -> pure [unitName <> ".service"]

    return . program "systemd-run" $
      ["--quiet","--user","--no-block","--no-ask-password", "--collect", "--send-sighup"] <>
      ["--scope" | unitScope] <>
      [x         | u      <- take 1 sdUnit, x <- ["--unit",  u]] <>
      [x         | Just s <- [unitSlice],  x <- ["--slice", s]] <>
      ["--"] <> uncurry (:) (showCmdSpec cspec)

-- Functions

showCmdSpec :: P.CmdSpec -> (String, [String])
showCmdSpec (P.RawCommand prog args) = (prog, args)
showCmdSpec (P.ShellCommand cmd)     = ("/bin/sh", ["-c", cmd])

-- | System.Posix.executeFile
exec :: HasCmd IO cmd => cmd -> IO ()
exec cmd = cmdSpec cmd >>= exec' where
  exec' (P.ShellCommand script)  = Posix.executeFile "/bin/sh" False ["-c", encodeString script] Nothing
  exec' (P.RawCommand prog args) = Posix.executeFile (encodeString prog) True (map encodeString args) Nothing

program :: FilePath -> [String] -> P.CmdSpec
program = P.RawCommand

shell :: String -> P.CmdSpec
shell = P.ShellCommand

term :: Terminal -> Run P.CmdSpec
term tc = Term tc Nothing

inTerm :: Terminal -> cmd -> Run cmd
inTerm tc cmd = Term tc (Just cmd)

-- | Like spawnPipe but not interpreting as shell
spawnPipeIO :: MonadIO m => IO () -> m (IO.Handle, Posix.ProcessID)
spawnPipeIO x = io $ do
    (rd, wd) <- Posix.createPipe
    Posix.setFdOption wd Posix.CloseOnExec True
    wh <- Posix.fdToHandle wd
    IO.hSetBuffering wh IO.LineBuffering
    cpid <- xfork $ Posix.dupTo rd Posix.stdInput >> x
    Posix.closeFd rd
    return (wh, cpid)

-- RUN

-- | Run external command and read its stdout.
readProcess :: MonadIO m => FilePath -> [String] -> m String
readProcess prog args = runProcess_ (P.RawCommand prog args) Nothing

readProcessWithInput :: MonadIO m => FilePath -> [String] -> String -> m String
readProcessWithInput prog args = runProcess_ (P.RawCommand prog args) . Just

runProcess_ :: (HasCmd m cmd, MonadIO m) => cmd -> Maybe String -> m String
runProcess_ cmd minput = do
  cspec <- cmdSpec cmd
  logCmdInfo "run" cspec

  let (prog, args) = showCmdSpec cspec
  let cp = (P.proc prog args) { P.std_in  = maybe P.NoStream (const P.CreatePipe) minput
                              , P.std_out = P.CreatePipe
                              , P.std_err = P.Inherit
                              }

  result <- io $ timeout (seconds 2) $ P.withCreateProcess cp $ \hin hout _ _ -> do
      whenJust minput $ \input ->
        whenJust hin $ \h ->
          IO.hPutStr h input >> IO.hClose h
      output <- maybe undefined IO.hGetContents hout
      length output `seq` return output

  io $ maybe (E.throwIO RunProcessTimeout) return result

-- LOGGING

logCmdInfo :: MonadIO m => String -> P.CmdSpec -> m ()
logCmdInfo s cmd = trace $ printf "%s: %s" s (uncurry P.showCommandForUser (showCmdSpec cmd))

-- SYSTEMD

sdRun :: String -> String -> a -> Run a
sdRun u i x = SdRun
  { unitName       = u
  , unitScope      = False
  , unitSlice      = Nothing
  , unitInstance   = i
  , unitIsTemplate = not (null i)
  , unitCmd        = x
  }

-- | systemd-escape
-- @ sdEscape ["--template=foo@.service"] ["bar/"] == ["foo@bar-.service"] @
sdEscape :: MonadIO m => [String] -> [String] -> m [String]
sdEscape opts args = lines <$> readProcess "systemd-escape" (opts ++ ("--":args))

-- Terminal options

topt :: String -> Maybe String -> Terminal
topt o v = def { terminalOpts = M.fromList [(o,v)] }

tCWD, tName, tSize :: String -> Terminal
tCWD  = topt "-cd"       . Just
tName = topt "-name"     . Just
tSize = topt "-geometry" . Just

tHold :: Terminal
tHold = topt "-hold" Nothing
