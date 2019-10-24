
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
  ( font
  , bfont
  , module SolarizedColorScheme
  , gsconfig1
  , xpConfig
  , xpConfigAuto
  , xpConfigNoHist
  ) where

import           Data.Ratio
import           Prelude
import           SolarizedColorScheme
import           XMonad
import qualified XMonad.Actions.GridSelect as GS
import qualified XMonad.Prompt             as XP
import qualified XMonad.Prompt.FuzzyMatch  as XP (fuzzyMatch)

-- 22 myFont, 32 myFontLarge
font :: Int -> String
font size
  | size <= 14 = "xft:xos4 Terminus:pixelsize=" ++ show size
  | otherwise  = "xft:TerminessTTF Nerd Font:pixelsize=" ++ show size

bfont :: Int -> String
bfont size = font size ++ ":style=bold"

gsconfig1 :: GS.HasColorizer a => GS.GSConfig a
gsconfig1 = def
  { GS.gs_cellwidth   = 360
  , GS.gs_cellheight  = 24
  , GS.gs_cellpadding = 5
  , GS.gs_navigate    = GS.navNSearch
  , GS.gs_font        = font 18
  , GS.gs_bordercolor = colCyan
  }

xpConfig :: XP.XPConfig
xpConfig = def
  { XP.font                = font 22
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
  , XP.completionKey       = (0, xK_Tab)
  , XP.changeModeKey       = xK_grave -- ` i.e. <S-#>
  , XP.promptKeymap        = XP.defaultXPKeymap
  , XP.showCompletionOnTab = False  -- only show list of completions when tab was pressed (False)
  , XP.searchPredicate     = XP.fuzzyMatch
  }

xpConfigAuto, xpConfigNoHist :: XP.XPConfig
xpConfigAuto   = xpConfig { XP.autoComplete = Just 500000 }
xpConfigNoHist = xpConfig { XP.historySize = 0 }
