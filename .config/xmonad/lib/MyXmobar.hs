
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

import           Prelude

import qualified Control.Exception             as E
import           Control.Monad
import Data.IORef
import           System.IO.Unsafe              (unsafePerformIO)

import qualified Xmobar                        as XB

import           XMonad
import qualified XMonad.StackSet               as W

import           XMonad.Actions.CopyWindow     (wsContainingCopies)
import           XMonad.Actions.WorkspaceNames (workspaceNamesPP)
import qualified XMonad.Config.Prime           as Prime
import qualified XMonad.Hooks.DynamicBars      as DBar
import           XMonad.Hooks.DynamicLog       (PP(..), xmobarColor, xmobarStrip)
import           XMonad.Hooks.UrgencyHook      (readUrgents)
import           XMonad.Util.Loggers           (Logger, logCurrent, logLayout, logSp, onLogger)

import           Codec.Binary.UTF8.String      (encodeString)

import           Graphics.X11.Xinerama         (getScreenInfo)

import           MyLayouts                     (isHidden)
import           MyRun
import           MyTheme
import           Util

-- Entry points

myStatusBars :: Prime.Prime l l
myStatusBars =
  (Prime.logHook Prime.=+ DBar.multiPPFormat runPP focusedPP unfocusedPP) Prime.>>
  (Prime.startupHook     Prime.=+ DBar.dynStatusBarStartup'   (dynStatusBar sbarHackRef) (partialCleanup sbarHackRef)) Prime.>>
  (Prime.handleEventHook Prime.=+ DBar.dynStatusBarEventHook' (dynStatusBar sbarHackRef) (partialCleanup sbarHackRef))

exitHook :: X ()
exitHook = io (cleanup sbarHackRef)

-- Xmobar

fg c = xmobarColor c ""

colLow    = colBase01
colNormal = colGreen
colHigh   = colOrange

xmobarConfig :: Int -> XB.Config
xmobarConfig screen = XB.defaultConfig
  { XB.font            = font 16
  , XB.additionalFonts = ["WenQuanYi Bitmap Song:pixelsize = 14"]
  , XB.bgColor  = colBase03
  , XB.fgColor  = colBase0
  , XB.position = XB.OnScreen screen XB.Top
  , XB.template = mconcat [tplLeft, "}", tplCenter, "{", tplRight]
  , XB.commands =
    [ XB.Run XB.StdinReader
    , XB.Run $ XB.Locks
    , XB.Run $ XB.Kbd [("fi", "qwerty-fi")]
    , XB.Run $ XB.Date (mconcat [ fg colBase01 "%a"
                                , " "
                                , fg colBase1 "%-d"
                                , "."
                                , fg colBase00 "%-m"
                                , " "
                                , fg colBase1  "%-H:%M"
                                , "."
                                , fg colBase01 "%S"
                                , " "
                                , fg colBase01 "%Z"
                                ]
                       ) "date" 10
    , XB.Run $ XB.DynNetwork  ["-t", "<tx>|<rx>"
                              ,"-L", "131072"
                              ,"-H", "1048576"
                              ,"-l", colLow
                              ,"-n", colNormal
                              ,"-h", colHigh
                              ] 50
    , XB.Run $ XB.MultiCpu    ["-t", "<total>|<system>"
                              ,"-L", "3"
                              ,"-H", "50"
                              ,"-l", colLow
                              ,"-n", colNormal
                              ,"-h", colHigh
                              ] 50
    , XB.Run $ XB.Memory      ["-t", fg "#859900" "<availableratio>" ] 50
    , XB.Run $ XB.TopProc     ["-t", "<name1>"] 50
    , XB.Run $ XB.TopMem      ["-t", "<name1> " <> fg colBase01 "<mem1>" <> " <name2> " <> fg colBase01 "<mem2>"] 50
    , XB.Run $ XB.Volume "default" "Master"
                              ["-t", "<volume>%"
                              ,"-L", "5"
                              ,"-H", "60"
                              ,"-p", "2"
                              ,"-l", colLow
                              ,"-n", colNormal
                              ,"-h", colHigh
                              ] 100
    , XB.Run $ XB.Com "cat" [ "/tmp/xmobar.ticker" ] "btcprice" 600
    ]
  }
    where
      tplLeft   = fg colBase1 "%kbd%" <> " %StdinReader%"
      tplCenter = fg colOrange "%locks%" -- <> " %wlp2s0wi%"
      tplRight  = sepByConcat (fg colBase01 " : ")
        [ "%top%"
        , "%multicpu%"
        , "%topmem%"
        , "%memory%"
        , "%dynnetwork%"
        , "%default:Master%"
        , "%EFHF%"
        , "%btcprice%"
        , "%battery%"
        , "%date%"
        ]

battery = XB.Battery
  ["-t", "<acstatus><left>% " <> fg colCyan "<timeleft>"
  ,"-L", "15"
  ,"-H", "80"
  ,"-l", colHigh
  ,"--"
  ,"-O", fg colGreen "AC" <> " "
  ,"-i", ""
  ,"-o", ""
  ] 100

-- wireless = XB.Wireless "wlp2s0" [ "-t", "<essid>", "-p", "2" ] 100

-- PP

focusedPP, unfocusedPP :: PP
focusedPP = def
  { ppUrgent          = fg colGreen
  , ppVisible         = fg colCyan
  , ppCurrent         = fg colMagenta
  , ppHidden          = fg colBase1
  , ppHiddenNoWindows = fg colBase01
  }

unfocusedPP = focusedPP { ppCurrent = fg colBlue }

-- TODO duplicate
tagKeys :: [String]
tagKeys = map (:[]) ['a'..'z']

ppKey     = fg colYellow . (++ ":")
ppCopies  = fg colYellow

runPP :: PP -> X String
runPP pp' = do
  pp <- workspaceNamesPP pp'
  maybe "" encodeString <$> mconcat [ logWS pp, logSp 1, logLayout, logSp 1, onLogger xmobarStrip logCurrent ]

-- Loggers

-- | Extends the DynamicLog.PP with different formatting for workspaces that contain copies of a window (see CopyWindow).
-- Also does custom formatting for key hints.
logWS :: PP -> Logger
logWS pp = do
  wset    <- gets windowset
  wsSort  <- ppSort pp
  urgents <- readUrgents
  copies  <- wsContainingCopies
  hiddens <- filterM (runQuery isHidden) (W.allWindows wset)
  return $ Just $ sepByConcat (ppWsSep pp)
    [ ppKey k <> ppWS wset copies urgents hiddens ws (W.tag ws)
      | (ws,k) <- wsSort (W.workspaces wset) `zip` tagKeys ]
  where
    ppWS :: WindowSet -> [WorkspaceId] -> [Window] -> [Window] -> WindowSpace -> String -> String
    ppWS wset copies urgents hiddens w
      | Just _ <- W.stack w >>= W.filter (`elem` urgents)         = (ppUrgent pp)
      | W.tag w == W.tag (W.workspace (W.current wset))           = (ppCurrent pp)
      | W.tag w `elem` map (W.tag . W.workspace) (W.visible wset) = (ppVisible pp)
      | W.tag w `elem` copies                                     = (ppCopies)
      | Just _ <- W.stack w >>= W.filter (`notElem` hiddens)      = (ppHidden pp)
      | otherwise                                                 = (ppHiddenNoWindows pp)


-- Dynamic bars

sbarHackRef :: IORef [(ScreenId, (Handle, ProcessID))]
sbarHackRef = unsafePerformIO (newIORef mempty)
{-# NOINLINE sbarHackRef #-}

dynStatusBar :: IORef [(ScreenId, (Handle, ProcessID))] -> DBar.DynamicStatusBar -- ~ ScreenId -> IO Handle
dynStatusBar ref screen@(S sid) = do
  screenInfo <- E.bracket (openDisplay "") closeDisplay getScreenInfo
  unless (check screenInfo) undefined
  r@(h,_) <- forkP $ XB.xmobar (xmobarConfig sid)
  atomicModifyIORef ref $ \xs -> ((screen, r) : xs, ())
  return h
    where
      check srs
        | r:rs <- drop sid srs = not $ any (r `containedIn`) (take sid srs ++ rs)
        | otherwise            = False

cleanup :: IORef [(ScreenId, (Handle, ProcessID))] -> IO ()
cleanup ref = readIORef ref >>= mapM_ (destroy . snd)

partialCleanup :: IORef [(ScreenId, (Handle, ProcessID))] -> ScreenId -> IO ()
partialCleanup ref sid = readIORef ref >>= mapM_ destroy . lookup sid

destroy (h, pId) = do
  E.try @E.IOException (hClose h)
  E.try @E.SomeException (MyRun.signalProcess MyRun.sigTERM pId) -- TODO SomeException
  E.try @E.SomeException (MyRun.getProcessStatus True False pId)
