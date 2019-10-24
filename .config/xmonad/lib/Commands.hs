{-# LANGUAGE GADTs #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}

------------------------------------------------------------------------------
-- |
-- Module      : Commands
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module Commands
  ( IsCmd(..)
  , AndThen(..)
  , Cmd
  , ck
  , ak
  , cmdsKeys
  , cmdGroup
  , keysOf
  , keysOf'
  , addKeys'
  , KeysConfig(..)
  , NamedAction
  , (^++^)
  ) where

import           Prelude

import           Data.Either                        (partitionEithers)
import           Data.Map                           (Map)
import qualified Data.Map                           as Map
import           Data.Typeable
import Data.Foldable
import Control.Applicative

import           XMonad

import qualified XMonad.Layout.BinarySpacePartition as BSP
import qualified XMonad.Layout.Magnifier            as Magnifier (MagnifyMsg(..))
import           XMonad.Util.EZConfig               (mkNamedKeymap)
import           XMonad.Util.NamedActions           (NamedAction(..), addName, getAction, showKm, submapDefaultName, (^++^))
import XMonad.Util.PureX

import           MyLayouts

-- orphan instances

deriving instance Show BSP.Rotate
deriving instance Show BSP.Swap
deriving instance Show BSP.ResizeDirectional
deriving instance Show BSP.TreeRotate
deriving instance Show BSP.TreeBalance
deriving instance Show BSP.FocusParent
deriving instance Show BSP.SelectMoveNode

-- IsCmd

class Typeable a => IsCmd a where
  command  :: a -> X ()
  describe :: a -> String

  default command :: Message a => a -> X ()
  command = sendMessage

  default describe :: Show a => a -> String
  describe = show

instance IsCmd (String, X ()) where
  describe = fst
  command  = snd

data AndThen c1 c2 = c1 :>> c2

instance (IsCmd c1, IsCmd c2) => IsCmd (AndThen c1 c2) where
  command  (c1 :>> c2) = command c1 >> command c2
  describe (c1 :>> c2) = describe c1 <> ", " <> describe c2

instance IsCmd ChangeLayout
instance IsCmd Resize
instance IsCmd IncMasterN
instance IsCmd ToggleStruts
instance Typeable t => IsCmd (Toggle t) where
  describe (Toggle t) = "Toggle " <> show (typeOf t)
instance IsCmd Navigate where
  describe (Go d2)       = "WinNav: Go "       <> show d2
  describe (Swap d2)     = "WinNav: Swap "     <> show d2
  describe (Move d2)     = "WinNav: Move "     <> show d2
  describe (Apply _f d2) = "WinNav: Apply f? " <> show d2
instance IsCmd MagnifyMsg where
  describe Magnifier.MagnifyLess = "Magnify (less)"
  describe Magnifier.MagnifyMore = "Magnify (more)"
  describe Magnifier.Toggle      = "Magnify (toggle)"
  describe Magnifier.ToggleOff   = "Magnify (off)"
  describe Magnifier.ToggleOn    = "Magnify (on)"
instance IsCmd BSP.Rotate
instance IsCmd BSP.Swap
instance IsCmd BSP.ResizeDirectional
instance IsCmd BSP.TreeRotate
instance IsCmd BSP.TreeBalance
instance IsCmd BSP.FocusParent
instance IsCmd BSP.SelectMoveNode

-- Cmd

data Cmd a where
  Key   :: forall a cmd. IsCmd cmd => EZKey -> cmd -> Cmd a
  Group :: String  -> Cmd a -> Cmd a
  Multi :: [Cmd a] -> Cmd a

instance Semigroup (Cmd a) where
  Multi [] <> a        = a
  a        <> Multi [] = a
  Multi xs <> Multi ys = Multi (xs <> ys)
  Multi xs <> a        = Multi (xs <> [a])
  a        <> Multi xs = Multi ([a] <> xs)
  a        <> b        = Multi [a,b]

instance Monoid (Cmd a) where
  mempty = Multi []

cmdGroup :: String -> Cmd a -> Cmd a
cmdGroup = Group

ck :: IsCmd cmd => String -> cmd -> Cmd a
ck = Key

ak :: String -> String -> X () -> Cmd a
ak k s x = ck k (s,x)

-- | @keysOf "M-r " [("j",D),("k",U)] "Description" someAction@
keysOf :: (Show v, Foldable t) => EZKey -> t (String, v) -> String -> (v -> X ()) -> Cmd a
keysOf k is nm = keysOf' k is . liftA2 (,) (\v -> nm <> " (" <> show v <> ")")

keysOf' :: (IsCmd cmd, Foldable t) => EZKey -> t (String, v) -> (v -> cmd) -> Cmd a
keysOf' k is f = fold [ck (k <> k') (f v) | (k',v) <- toList is]

-- Keys

cmdsKeys :: Cmd a -> [(EZKey, NamedAction)]
cmdsKeys = go [] where
  go _ (Key k c)    = (k, addName (describe c) (command c)) : []
  go g (Group g' c) = go (g ++ [g']) c
  go g (Multi cs)   = foldMap (go g) cs

type EZKey = String

data KeysConfig = KeysConfig
  { showKeys         :: String -> [String] -> X () -- ^ prints to stdout by default
  , showKeysKey      :: EZKey -- ^ @ M-<F1> @
  , showKeysTitle    :: String -- ^ Keys
  , showKeysTitleSub :: EZKey -> String -- ^ "Submap: " <>
  }

instance Default KeysConfig where
  def = KeysConfig pr "M-<F1>" "Keys" ("Submap " <>) where
    pr tt xs = io $ putStrLn $ unlines $ tt : xs

addKeys' :: KeysConfig -> [(EZKey, NamedAction)] -> XConfig l -> XConfig l
addKeys' kc ks = \xc -> xc { keys = \xc' -> fromKeymap kc ks xc' <> keys xc xc' }

fromKeymap :: KeysConfig -> [(EZKey, NamedAction)] -> XConfig Layout -> Map (KeyMask, KeySym) (X ())
fromKeymap KeysConfig{..} ks xc = Map.map getAction $ Map.fromList res
  where
    res  = go2 $ go1 ks'
    res' = go2 $ (\(as,_) -> (as,[])) <$> go1 ks'
    ks'  = ks ^++^ [(showKeysKey, showKeys showKeysTitle $ showKm res')]
    go1  = Map.map partitionEithers . Map.fromListWith (<>) . map f
    go2  = mkNamedKeymap xc . Map.toList . Map.mapWithKey g

    f :: (EZKey, a) -> (EZKey, [Either a (EZKey, a)])
    f (k,a)
      | (k1,' ':k2) <- span (/= ' ') k = (k1, [Right (k2, a)])
      | otherwise                      = (k,  [Left a])

    g :: EZKey -> ([NamedAction], [(EZKey, NamedAction)]) -> NamedAction
    g _ (a:_,  _   ) = a
    g k ([] ,  subk) = submapDefaultName <$> addName tt . showKeys tt . showKm <*> ask $ go2 $ go1 subk
      where tt = showKeysTitleSub k
