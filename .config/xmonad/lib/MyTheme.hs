{-# LANGUAGE RecordWildCards #-}

------------------------------------------------------------------------------
-- |
-- Module      : MyTheme
-- Description : Short description
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@relexsolutions.com>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module MyTheme
  ( sepByConcat
  , font
  , bfont
  , module XMonad.Config.Solarized
  , gsconfig1
  , xpConfig
  , xpConfigAuto
  , xpConfigNoHist
  -- * Xmobar
  , xmobarFont
  , xmobarConfig_
  , xbKbd
  , xbDate
  , xbBattery
  , xbMpd
  , xbWireless
  , xbDynNetwork, xbMultiCpu, xbMemory, xbTopProc, xbTopMem
  , xbVolume
  -- ** PP
  , runPP
  , focusedPP
  , unfocusedPP
  , colLow
  , colNormal
  , colHigh
  , ppKey
  , ppCopies
  ) where

import           XMonad                        hiding (Font, title)
import qualified XMonad                        as X
import           XMonad.Actions.CopyWindow     (wsContainingCopies)
import qualified XMonad.Actions.GridSelect     as GS
import           XMonad.Actions.WorkspaceNames (workspaceNamesPP)
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageHelpers    (isInProperty)
import           XMonad.Hooks.UrgencyHook      (readUrgents)
import qualified XMonad.Prompt                 as XP
import           XMonad.Prompt.FuzzyMatch      (fuzzyMatch, fuzzySort)
import qualified XMonad.StackSet               as W
import           XMonad.Util.Loggers

import           XMonad.Config.Solarized

import qualified Xmobar

import           Codec.Binary.UTF8.String      (encodeString)
import           Control.Monad
import qualified Data.Char                     as Char
import           Data.Foldable
import qualified Data.List
import           Data.Ratio
import           Prelude
import           Text.Printf                   (printf)

-- "Data.List.intersperse" generalized for any "Foldable" of some "Monoid".
sepByConcat :: (Monoid a, Foldable t) => a -> t a -> a
sepByConcat a = mconcat . Data.List.intersperse a . toList

terminus           = def { fontFamily = "xos4 Terminus" }
terminessPowerline = def { fontFamily = "xos4 Terminess Powerline" }
terminessNerd      = def { fontFamily = "TerminessTTF Nerd Font" }
wqyMicroHei size   = def { fontFamily = "WenQuanYi Micro Hei", fontSize = Just (PixelSize size) }

font, bfont :: Int -> Font
font size = sel { fontSize = Just (PixelSize size) } where
  sel | size <= 14 = terminessPowerline
      | otherwise  = terminessNerd
bfont size = (font size) { fontStyle = Just Bold }

data Font = Font
  { fontFamily :: String
  , fontSize   :: Maybe FontSize
  , fontStyle  :: Maybe FontStyle
  } deriving (Eq)

instance Show Font where
  show Font{..} = "xft:" <> fontFamily <> maybe "" dSize fontSize <> dStyle fontStyle where
    dSize (PixelSize s) = printf ":pixelsize=%i" s
    dSize (PointSize s) = printf ":size=%i" s
    dStyle s = case maybe "" show s of
                 x:xs -> ':' : x : [r | c<-xs, r <- [' '|Char.isUpper c]++[c]]
                 []   -> ""

instance X.Default Font where
  def = Font "monospace" def def

data FontSize  = PixelSize Int | PointSize Int
  deriving (Eq)

data FontStyle = Thin | Medium | Regular | Bold | BoldItalic | Light | LightItalic | Semibold | Italic
  deriving (Eq, Show)

gsconfig1 :: GS.HasColorizer a => GS.GSConfig a
gsconfig1 = def
  { GS.gs_cellwidth   = 360
  , GS.gs_cellheight  = 24
  , GS.gs_cellpadding = 5
  , GS.gs_navigate    = GS.navNSearch
  , GS.gs_font        = show (font 18)
  , GS.gs_bordercolor = colCyan
  }

xpConfig :: XP.XPConfig
xpConfig = def
  { XP.font                = show (font 18)
  , XP.bgColor             = colBase03
  , XP.fgColor             = colBase1
  , XP.fgHLight            = colMagenta
  , XP.bgHLight            = colBase02
  , XP.borderColor         = colCyan
  , XP.promptBorderWidth   = 1
  , XP.position            = XP.CenteredAt (1%3) (1%2)
  , XP.height              = 35 -- per row
  , XP.maxComplRows        = Just 40
  , XP.historySize         = 512
  , XP.historyFilter       = XP.deleteAllDuplicates
  , XP.completionKey       = (0, X.xK_Tab)
  , XP.changeModeKey       = X.xK_grave -- ` i.e. <S-#>
  , XP.promptKeymap        = XP.defaultXPKeymap
  , XP.showCompletionOnTab = False  -- only show list of completions when tab was pressed (False)
  , XP.searchPredicate     = fuzzyMatch
  , XP.sorter              = fuzzySort
  }

xpConfigAuto, xpConfigNoHist :: XP.XPConfig
xpConfigAuto   = xpConfig { XP.autoComplete = Just 500000 }
xpConfigNoHist = xpConfig { XP.historySize = 0 }

fg c = xmobarColor c ""

colLow, colHigh, colNormal :: String
colLow    = colBase01
colNormal = colGreen
colHigh   = colOrange

focusedPP, unfocusedPP :: PP
focusedPP = def
  { ppUrgent          = fg colGreen
  , ppVisible         = fg colCyan
  , ppCurrent         = fg colMagenta
  , ppHidden          = fg colBase1
  , ppHiddenNoWindows = fg colBase01
  , ppTitle           = xmobarFont 1 . shorten 64 . xmobarStrip
  }
unfocusedPP = focusedPP { ppCurrent = fg colBlue }

ppKey, ppCopies :: String -> String
ppKey     = fg colYellow . (++ ":")
ppCopies  = fg colYellow

xmobarConfig_ :: Xmobar.Config
xmobarConfig_ = Xmobar.defaultConfig
  -- { Xmobar.font            = show (font 16)
  -- , Xmobar.additionalFonts = [show (wqyMicroHei 16)]
  { Xmobar.font            = show (terminessPowerline { fontSize = Just (PixelSize 12) })
  , Xmobar.additionalFonts = [show (terminessPowerline { fontSize = Just (PixelSize 12) })]
  , Xmobar.bgColor         = colBase03
  , Xmobar.fgColor         = colBase0
  , Xmobar.template = tplLeft <> "} %xmonad% { " <> tplRight
  , Xmobar.commands =
    [ Xmobar.Run Xmobar.Locks
    , Xmobar.Run xbKbd
    , Xmobar.Run $ xbDate 10
    , Xmobar.Run $ xbDynNetwork 50
    , Xmobar.Run $ xbMultiCpu 50
    , Xmobar.Run $ xbMemory 50
    , Xmobar.Run $ xbTopProc 50
    , Xmobar.Run $ xbTopMem 50
    , Xmobar.Run $ xbVolume "default" "Master" 100
    , Xmobar.Run $ Xmobar.PipeReader "xmonad:/dev/fd/0" "xmonad"
    , Xmobar.Run $ Xmobar.Com "cat" [ "/tmp/xmobar.ticker" ] "btcprice" 600
    , Xmobar.Run $ xbMpd 50
    ]
  }
    where
      tplLeft = sepByConcat (fg colBase01 " : ")
        [ "%mpd%"
        , fg colBase1 "%kbd%" <> " " <> fg colOrange "%locks%"
        ]
      tplRight = sepByConcat (fg colBase01 " : ")
        [ "%multicpu% %top%"
        , "%memory% %topmem%"
        , "%dynnetwork%"
        , "%default:Master%"
        , "%btcprice%"
        -- , "%battery%"
        , "%date%"
        ]

xmobarFont :: Int -> String -> String
xmobarFont i = wrap (printf "<fn=%i>" i) "</fn>"

xbKbd :: Xmobar.Kbd
xbKbd = Xmobar.Kbd [("fi", "qwerty-fi")]

xbDate :: Xmobar.Rate -> Xmobar.Date
xbDate = Xmobar.Date fmt "date"
  where fmt = unwords [ fg colBase01 "%a", fg colBase1 "%-d"<>"."<>fg colBase00 "%-m", fg colBase1 "%-H:%M"<>"."<>fg colBase01 "%S %Z"
                      , "(week %V)"
                      ]

xbBattery :: Xmobar.Rate -> Xmobar.Monitors
xbBattery = Xmobar.Battery
  [ "-t", "<acstatus><left>% " <> fg colCyan "<timeleft>"
  , "-L", "15"
  , "-H", "80"
  , "-l", colHigh
  , "--"
  , "-O", fg colGreen "AC" <> " "
  , "-i", ""
  , "-o", ""
  ]

xbMpd :: Xmobar.Rate -> Xmobar.Monitors
xbMpd = Xmobar.MPD
  [ "-t", xmobarFont 1 $ printf "%s: %s (%s) <statei> [%s]" artist title album flags
  , "--"
  , "-P", fg colGreen  ">>"
  , "-Z", fg colYellow "||"
  , "-S", fg colOrange "><"
  ] where
    title  = fg colBase1 "<title>"
    artist = fg colCyan "<artist>"
    album  = fg colBlue "<album>"
    flags  = fg colBase1 "<flags>"

-- | @wireless "wlp2s0"@
xbWireless :: Xmobar.Interface -> Xmobar.Rate -> Xmobar.Monitors
xbWireless iface = Xmobar.Wireless iface [ "-t", "<essid>", "-p", "2" ]

xbDynNetwork, xbMultiCpu, xbMemory, xbTopProc, xbTopMem :: Xmobar.Rate -> Xmobar.Monitors

xbDynNetwork = Xmobar.DynNetwork
  [ "-t", "<tx>|<rx>"
  , "-L", "131072"
  , "-H", "1048576"
  , "-l", colLow
  , "-n", colNormal
  , "-h", colHigh
  ]

xbMultiCpu = Xmobar.MultiCpu
  [ "-t", "<total>|<system>"
  , "-L", "3"
  , "-H", "50"
  , "-l", colLow,"-n", colNormal,"-h", colHigh
  ]

xbMemory = Xmobar.Memory
  [ "-t", fg "#859900" "<availableratio>" ]

xbTopProc = Xmobar.TopProc
  ["-t", "<name1>"]

xbTopMem = Xmobar.TopMem
  [ "-t", "<name1> " <> fg colBase01 "<mem1>" <> " <name2> " <> fg colBase01 "<mem2>" ]

-- | %<mixer>:<element>%
xbVolume :: String -> String -> Xmobar.Rate -> Xmobar.Monitors
xbVolume mixer element = Xmobar.Volume mixer element
  [ "-t", "<volume>%"
  , "-L", "5"
  , "-H", "60"
  , "-p", "2"
  , "-l", colLow
  , "-n", colNormal
  , "-h", colHigh
  ]

-- PP

runPP :: PP -> X String
runPP pp' = do
  pp <- workspaceNamesPP pp'
  maybe "" encodeString <$> mconcat
    [ logWS pp
    , logSp 1
    , ppLayout pp `onLogger` logLayout
    , logSp 1
    , ppTitle pp `onLogger` logTitle
    ]

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

    isHidden :: Query Bool
    isHidden  = isInProperty "_NET_WM_STATE" "_NET_WM_STATE_HIDDEN"

    -- TODO duplicate
    tagKeys :: [String]
    tagKeys = map (:[]) ['a'..'z']
