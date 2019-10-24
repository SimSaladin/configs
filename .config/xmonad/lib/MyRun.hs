{-# LANGUAGE NoImplicitPrelude #-}
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
------------------------------------------------------------------------------

module MyRun (
  -- * System.IO
  IO.hClose,
  IO.hPutStrLn,
  IO.stdout,
  IO.stderr,
  IO.Handle,

  -- * System.Process
  spawnCP,
  cleanupCP,
  P.showCommandForUser,
  P.CreateProcess(..),
  P.ProcessHandle,

  -- * System.Posix
  forkP,
  statusP,
  Posix.getProcessStatus,
  Posix.signalProcess,
  Posix.sigTERM,
  Posix.ProcessID,
  Posix.ProcessGroupID,
  Posix.Signal,
  Posix.ProcessStatus(..),

  -- * System.Exit
  Exit.exitSuccess,
  Exit.ExitCode(..),

  -- * Logging
  logInfo,
  logError,
  logErrorShow,

  -- * Run (sync)
  runWithInput,

  -- * Spawn (async)
  spawn,
  spawnS,
  spawnT,
  spawnTS,
  spawnT_,
  spawnSP,

  -- * Systemd
  service,
  systemdEscape,

  -- * Terminal
  Terminal(windowName, windowGeometry),
  terminalHold,
  dialog,

  -- * Tmux
  tmux,
  tmuxp,
  tmuxpComplFun,
  tmuxPrompt,

  -- * Browser
  chromeApp,
  qutebrowser,
  qutebrowserP,
  qutebrowserCompl,

  -- * Misc
  mpv_clipboard,
  mpc,
  volume,
  mic,
  backlight,
  actionPrompt,
  inputPromptProg,
  inputPromptWithHistCompl,
  ) where

import           Prelude
import qualified Control.Exception        as E
import           Control.Monad            (forM)
import qualified Data.Char                as Char
import           Data.Functor             (($>))
import qualified Data.List                as List
import           Data.Maybe               (maybeToList)
import qualified System.Directory         as Directory
import qualified System.Exit              as Exit
import qualified System.IO                as IO
import qualified System.Posix             as Posix
import qualified System.Process           as P

import           XMonad                   hiding (spawn)
import qualified XMonad.Prompt            as XP
import qualified XMonad.Prompt.FuzzyMatch as XP.FuzzyMatch
import qualified XMonad.Prompt.Input      as XP.Input
import qualified XMonad.Prompt.Shell      as XP.Shell
import qualified XMonad.Util.Run          as XRun

import           Util

-- * Run (sync)

runWithInput :: MonadIO m => FilePath -> [String] -> String -> m String
runWithInput prog args input = do
  logInfo ("run(in+read): " ++ P.showCommandForUser prog args)
  XRun.runProcessWithInput prog args input

systemdEscape :: MonadIO m => String -> m String
systemdEscape s = takeWhile (not . Char.isSpace) <$> runWithInput "systemd-escape" ["--", s] ""

-- * Spawn (async)

-- | Execute program (no shell interpolation).
spawn :: FilePath -> [String] -> X ()
spawn prog args = logInfo ("spawn: " ++ P.showCommandForUser prog args) >> XRun.safeSpawn prog args

-- | Run shell command.
spawnS :: String -> X ()
spawnS cmd = logInfo ("spawnS: " ++ cmd) >> XRun.unsafeSpawn cmd

-- | Execute program in terminal.
spawnT :: Terminal -> FilePath -> [String] -> X ()
spawnT Terminal{..} prog args = do
  t <- asks (terminal . config)
  spawn t $ concat $
    [["-name",     windowName      ] | windowName       /= ""] ++
    [["-geometry", windowGeometry  ] | windowGeometry   /= ""] ++
    [["-cd",       workingDirectory] | workingDirectory /= ""] ++
    [["-hold"                      ] | hold                  ] ++
    [options] ++
    ["-e" : prog : args | prog /= ""]

-- | Open terminal (default command, usually shell interpreter).
spawnT_ :: Terminal -> X ()
spawnT_ t = spawnT t "" []

-- | Run shell command in a terminal window.
spawnTS :: Terminal -> String -> X ()
spawnTS t cmd = spawnT t "/bin/bash" ["-c", cmd]

-- | Run shell command with output to pipe.
spawnSP :: MonadIO m => String -> m IO.Handle
spawnSP cmd = logInfo ("spawn(sh+pipe): " ++ cmd) >> XRun.spawnPipe cmd

spawnCP :: MonadIO m => P.CreateProcess -> m (Maybe IO.Handle, Maybe IO.Handle, Maybe IO.Handle, P.ProcessHandle)
spawnCP cp = logInfo ("spawn:CreateProcess:" ++ show cp) >> io (P.createProcess cp)

cleanupCP :: MonadIO m => (Maybe IO.Handle, Maybe IO.Handle, Maybe IO.Handle, P.ProcessHandle) -> m ()
cleanupCP = io . P.cleanupProcess

forkP :: MonadIO m => IO () -> m (IO.Handle, Posix.ProcessID)
forkP child = do
  (pread, pwrite) <- io Posix.createPipe
  pID <- io . Posix.forkProcess $ do
      uninstallSignalHandlers
      Posix.createSession
      Posix.dupTo pread Posix.stdInput
      child
  h <- io $ Posix.fdToHandle pwrite
  io $ IO.hSetBuffering h IO.LineBuffering
  return (h, pID)

statusP :: MonadIO m => Posix.ProcessID -> m (Maybe Posix.ProcessStatus)
statusP = io . Posix.getProcessStatus False False

-- | @service name prog args@
service :: MonadIO m => Maybe String -> Maybe String -> FilePath -> [String] -> m ()
service name i prog args = do
  logInfo ("service: " ++ show name ++ "@" ++ show i ++ ": " ++ P.showCommandForUser prog args)
  i' <- forM i systemdEscape
  let munit = ["--unit=" ++ nm ++ maybe "" ("@"++) i' | nm <- maybeToList name]
  XRun.safeSpawn "systemd-run" $ ["--user", "--collect"] ++ munit ++ (prog : args)

-- | Note: google-chrome as single process is really single process (without
-- sandboxing). So if starting with that unit name fails, we just launch chrome
-- directly and it'll open a new window in existing session.
--
-- Some wrapper and systemd-socket solution could perhaps be better.
--
-- Also, chrome likes to stay in the background even after you close
-- all its windows. For that, uncheck the "Continue running background apps when
-- Chrome is closed" flag in chrome://settings.
chromeApp :: String -> X ()
chromeApp url = service (Just "google-chrome") (Just url) "google-chrome-stable" ["--app=" ++ url]

-- | --qt-arg name app_name etc. https://peter.sh/experiments/chromium-command-line-switches/
-- $XDG_RUNTIME_DIR/qutebrowser/$session/runtime/ipc-*
-- /usr/share/qutebrowser/scripts/open_url_in_instance.sh
qutebrowser :: String -> X ()
qutebrowser "" = return ()
qutebrowser p  = service (Just "qutebrowser") (Just p) "qutebrowser" ["-r", p]

-- * quteb

qutebrowserP :: XP.XPConfig -> String -> X (Maybe String)
qutebrowserP xpc nm = io qutebrowserCompl >>= XP.Input.inputPromptWithCompl xpc nm

qutebrowserCompl :: IO (String -> IO [String])
qutebrowserCompl =
  Directory.getXdgDirectory Directory.XdgData "qutebrowser" >>=
    E.try . Directory.listDirectory >>=
      either @IOError (\e -> logErrorShow e $> []) (pure . f) >>=
        pure . XP.mkComplFunFromList'
  where
    f = filter $ \x -> not
      $ ("-qutebrowser" `List.isSuffixOf` x)
      || ("." `List.isPrefixOf` x)
      || (x `elem` ["null", "userscripts", "qtwebengine_dictionaries", "blocked-hosts"])

-- * Terminal

-- | Use the "Default" instance to construct 'Terminal' values.
data Terminal = Terminal
  { windowName       :: String -- ^ @-name name@
  , windowGeometry   :: String  -- ^ e.g. 130x40
  , workingDirectory :: String -- ^ Set @-cd directory@
  , hold             :: Bool -- ^ Don't automatically destroy the terminal window. @-hold@
  , options          :: [String] -- ^ Arbitrary arguments for the terminal program
  } deriving (Eq, Show)

instance Default Terminal where
  def = Terminal "" "" "" False []

terminalHold :: Terminal -> Terminal
terminalHold t = t { hold = True }

terminalOpts :: [String] -> Terminal -> Terminal
terminalOpts xs t = t { options = options t ++ xs }

dialog :: Terminal
dialog = def
  { windowName = "dialog"
  , windowGeometry = "130x40"
  }

-- * Tmux

tmux, tmuxp :: Terminal -> String -> X ()
tmuxp t session = spawnT (terminalOpts ["-sl","0"] t) "tmuxp" ["load", "-y", session]
tmux  t session = spawnT (terminalOpts ["-sl","0"] t) "tmux"  ["new-session", "-A", "-s", session]

data TmuxPrompt = TmuxPrompt

instance XP.XPrompt TmuxPrompt where
  showXPrompt TmuxPrompt = "TMUX: "
  nextCompletion _ x xs = XP.getNextCompletion x (map (takeWhile (/='\x1F')) xs)
  commandToComplete   _ = filter (/='\x1F')
  completionToCommand _ = takeWhile (/='\x1F')

-- | Prompt for a known tmuxp session or a tmux session.
tmuxPrompt :: XP.XPConfig -> X ()
tmuxPrompt xpc = do
  comp <- io tmuxpComplFun
  XP.mkXPromptWithReturn TmuxPrompt xpc (pure . comp) return XP.Input.?+ (\is -> (if null (comp is) then tmux def else tmuxp def) is)

-- | tmux display-message -p '#{S:#{?session_attached,#{session_name} ,}}'
tmuxpComplFun :: IO (String -> [String])
tmuxpComplFun = do
  sessions <- getAll
  return $ \case
    []  -> sessions
    str -> filter (XP.FuzzyMatch.fuzzyMatch str . filter (/= sep)) sessions
  where
    getAll = do
      known <- getKnown
      attached <- getAttached
      let attachedKnown = map (takeWhile (/= sep)) attached
      return $ attached ++ filter (`notElem` attachedKnown) known

    getKnown =
      Directory.getXdgDirectory Directory.XdgConfig "tmuxp" >>=
        E.try . Directory.listDirectory >>=
          either @IOError (\e -> logErrorShow e $> []) (pure . f)
      where
        f = List.filter (not . null) . map (takeWhile (/= '.')) . List.filter (List.isSuffixOf ".yaml")

    getAttached :: IO [String]
    getAttached = lines <$> runWithInput "tmux"
      [ "list-sessions", "-F", "#S" ++ [sep] ++ " [#{W:#{E:window-status-format} ,#{E:window-status-current-format} }] #{?session_attached,(attached),}" ]
      ""

    sep  = '\x1F'

-- * Random

mpv_clipboard :: X ()
mpv_clipboard = spawnS "exec mpv --really-quiet --profile=preview \"$(xclip -o)\" &>/dev/null"

mpc :: String -> X ()
mpc cmd = spawn "mpc" [cmd]

volume :: Int -> X ()
volume d = spawn "pactl" ["set-sink-volume", "@DEFAULT_SINK@", s]
  where s | d >= 0    = "+" ++ show d ++ "%"
          | otherwise = show d ++ "%"

mic :: String -> X ()
mic s = spawn "pactl" ["set-source-volume", "@DEFAULT_SOURCE@", s]

backlight :: Int -> X ()
backlight d = spawn "xbacklight" $ if d >= 0 then ["-inc", show d] else ["-dec", show (-d)]

-- * XP

inputPromptWithHistCompl :: XP.XPConfig -> String -> X (Maybe String)
inputPromptWithHistCompl xpc name = XP.Input.inputPromptWithCompl xpc name (XP.historyCompletionP (== name ++ ": "))

inputPromptProg :: XP.XPConfig -> String -> X (Maybe String)
inputPromptProg xpc name = do
  cmds <- io XP.Shell.getCommands
  let cfun = XP.Shell.getShellCompl cmds (XP.searchPredicate xpc)
  XP.Input.inputPromptWithCompl xpc name cfun

actionPrompt :: XP.XPConfig -> String -> [(String, X ())] -> X ()
actionPrompt xpc t as = actionPrompt' xpc t as

actionPrompt' :: XP.XPConfig -> String -> [(String, X ())] -> X ()
actionPrompt' xpc t as = XP.mkXPrompt p xpc cfun go
  where
    p = ActionsXP t as
    go x = maybe (pure ()) id ( lookup x as )
    cfun = \x -> return $ filter (XP.searchPredicate xpc x) (map fst as)

data ActionsXP = ActionsXP
  { axpTitle   :: String
  , axpActions :: [(String, X ())]
  }

instance XP.XPrompt ActionsXP where
  showXPrompt ActionsXP{..}        = axpTitle <> ": "
  -- nextCompletion ActionsXP{..}     = XP.getNextCompletion
  -- commandToComplete _              = id
  -- completionFunction ActionsXP{..} = \x -> if null x then return (map fst axpActions) else XP.mkComplFunFromList (map fst axpActions) x
