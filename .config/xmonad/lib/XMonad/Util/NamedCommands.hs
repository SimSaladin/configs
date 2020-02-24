{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DefaultSignatures         #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeOperators             #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-redundant-constraints #-}
------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Util.NamedCommands
-- Description : Named commands
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module XMonad.Util.NamedCommands
  (
  -- IsCmd(..),
  -- SomeCmd,
  -- SendMessage(..),
  -- (:??),
  -- (:>>)(..),
  -- CmdPrompt(..), cmdPrompt, cmdPrompt',
  module XMonad.Util.NamedCommands
  ) where

import           XMonad
import qualified XMonad.Prompt                       as XP
import           XMonad.Util.NamedActions           (NamedAction)
import qualified XMonad.Util.NamedActions           as NA

import           XMonad.Hooks.ManageDocks           (ToggleStruts(..))
import qualified XMonad.Layout.BinarySpacePartition as BSP
import           XMonad.Layout.Magnifier            (MagnifyMsg)
import qualified XMonad.Layout.Magnifier            as Magnifier (MagnifyMsg(..))
import           XMonad.Layout.MultiToggle          (Toggle(Toggle))
import           XMonad.Layout.WindowNavigation     (Navigate(Apply, Go, Move, Swap))

import           Prelude
import           Control.Applicative

import           Data.Data
import           Data.Kind
import           Data.Proxy
import Text.Printf (printf)

import           GHC.TypeLits

--------------------------------------------------------------
-- Types
--------------------------------------------------------------

data SomeCmd = forall a. IsCmd a => SomeCmd a

class IsCmd a where
  toCmd :: a -> SomeCmd
  toCmd = SomeCmd

  command :: a -> NamedAction
  default command :: (Show a, Message a) => a -> NamedAction
  command = NA.sendMessage'

  describeType :: a -> String
  default describeType :: Typeable a => a -> String
  describeType = show . typeOf

  cmdEnum :: a -> [a]
  default cmdEnum :: Data a => a -> [a]
  cmdEnum _ = enumConstrs

data SendMessage msg = SendMessage !msg
  deriving Show

-- | Combined commands (sequential)
data c :>> d = c :>> d

-- | Named command group (list).
--
-- @ type LayoutBSPCmd = "BSP" :?? '[BSP.Rotate, BSP.Swap, BSP.ResizeDirectional, BSP.TreeRotate, BSP.TreeBalance, BSP.FocusParent, BSP.SelectMoveNode] @
data (name :: Symbol) :?? (commands :: [Type]) = CmdL SomeCmd

--------------------------------------------------------------
-- Misc.
--------------------------------------------------------------

infixr 0 ?
infixr 2 .?

-- | Assign name to arbitrary X action.
--
-- @termLauncher ? "Launch my terminal"@
--
-- @flip NA.addName@
(?) :: X () -> String -> NamedAction
(?) = flip NA.addName

-- | For building a @NamedAction@ from some @a@ by two functions:
-- A @a -> X ()@ to give the action, and @a -> String@ to give a name.
--
-- @liftA2 (?)@
(.?) :: (a -> X ()) -> (a -> String) -> a -> NamedAction
(.?) = liftA2 (?)

--------------------------------------------------------------
-- instance IsCmd
--------------------------------------------------------------

instance (Show a, Message a, Data a) => IsCmd (SendMessage a) where
  command (SendMessage a) = NA.sendMessage' a
  cmdEnum _ = SendMessage <$> enumConstrs

instance IsCmd NamedAction where
  command        = id
  describeType _ = show (typeOf (pure () :: X ()))
  cmdEnum _      = []

instance IsCmd SomeCmd where
  toCmd                    = id
  command      (SomeCmd x) = command x
  describeType (SomeCmd x) = describeType x
  cmdEnum      (SomeCmd x) = SomeCmd <$> cmdEnum x

instance (IsCmd c, IsCmd d) => IsCmd (c :>> d) where
  command (c :>> d) = let a = command c
                          b = command d
                       in command $ NA.NamedAction (NA.getAction a >> NA.getAction b, [unwords $ NA.showName a ++ [">>"] ++ NA.showName b])
  describeType (c :>> d) = describeType c <> ", " <> describeType d
  cmdEnum _ = [] -- TODO

instance forall name. KnownSymbol name => IsCmd (name :?? '[]) where
  command      _ = NA.noName (return ())
  describeType _ = symbolVal (Proxy :: Proxy name)
  cmdEnum      _ = []

instance forall name x xs. (KnownSymbol name, IsCmd x, IsCmd (name :?? xs)) => IsCmd (name :?? (x ': xs)) where
  command (CmdL (SomeCmd x)) = command x
  describeType _ = symbolVal (Proxy :: Proxy name)
  cmdEnum _ = map (CmdL . SomeCmd) (cmdEnum undefined :: [x]) <> map (\(CmdL c) -> CmdL c) (cmdEnum undefined :: [name :?? xs])

--------------------------------------------------------------
-- TODO
--------------------------------------------------------------

-- | Action
data Action (sym :: Symbol) (a :: Type)

-- | Parameter
data Param (sym :: Symbol) (a :: Type)

-- | Nesting
data (a :: k) :|>> (b :: Type)
infixr 4 :|>>

-- | Alternative
data a :||| b
infixr 3 :|||

--data (a :: Type) :|->| (f :: Type -> Type) = CF

class HasCmd decl where
  type MkCmd decl (a :: Type)

  mkCmd :: a -> Proxy decl -> MkCmd decl a

instance (KnownSymbol sym, HasCmd sub) => HasCmd (Param sym param :|>> sub) where
  type MkCmd (Param sym param :|>> sub) a = param -> MkCmd sub a
  mkCmd a _ =
    \p -> mkCmd a (Proxy :: Proxy sub)

instance (KnownSymbol sym) => HasCmd (Action sym res) where
  type MkCmd (Action sym res) a = a
  mkCmd a _ = a

-- mkCmd (_ :: a) (Proxy :: Proxy RotStack) :: RotateAction -> RotateFunction -> a
type RotStack =
  Param "A" RotateAction :|>> Param "F" RotateFunction :|>> Action "Rotate Stack" (X ())

data RotateAction   = RotAll | RotSlaves deriving Show
data RotateFunction = RotU | RotD deriving Show

type Rot' = MkCmd RotStack ()

-- rotcmd :: RotateStackCmd
-- rotcmd = CT (CP RotAll RotU)

--------------------------------------------------------------
-- instance Show
--------------------------------------------------------------

instance Show SomeCmd where
  show (SomeCmd cmd) = describe cmd

instance Show (s :?? cl) where
  show (CmdL s) = show s

--------------------------------------------------------------
-- Generics
--------------------------------------------------------------

enumConstrs :: forall a. Data a => [a]
enumConstrs = case dataTypeRep (dataTypeOf @a undefined) of
                AlgRep cs -> cs >>= fromConstrM enumConstrs
                _         -> []

--------------------------------------------------------------
-- Prompt
--------------------------------------------------------------

data CmdPrompt = CmdPrompt String [SomeCmd]

instance XP.XPrompt CmdPrompt where
  showXPrompt (CmdPrompt s _) = printf "%s: " s
  nextCompletion     _ = XP.getNextCompletion
  commandToComplete  _ = id

-- | @ cmdPrompt xpconfig (Proxy @MagnifyMsg) @
cmdPrompt :: forall a. (IsCmd a) => XP.XPConfig -> Proxy a -> NamedAction
cmdPrompt xpc p = mkCmdPrompt prompt xpc ? printf "Prompt (cmd: %s)" (describeType cmd)
  where
    prompt = CmdPrompt nm xs
    cmd    = undefined `asProxyTypeOf` p
    nm     = printf "CMD (%s)" (describeType cmd)
    xs     = SomeCmd <$> cmdEnum cmd

mkCmdPrompt :: CmdPrompt -> XP.XPConfig -> X ()
mkCmdPrompt prompt@(CmdPrompt title cmds) xpc = XP.mkXPrompt prompt xpc cf gf
  where
    xs   = [(describe x, NA.getAction x) | x <- map (\(SomeCmd x) -> command x) cmds]
    cf s = pure $ filter (XP.searchPredicate xpc s) (map fst xs)
    gf s = mapM_ id (lookup s xs)

describe :: IsCmd a => a -> String
describe = unwords . NA.showName . command

--------------------------------------------------------------
-- instance (orphans)
--------------------------------------------------------------

instance IsCmd Navigate where
  command msg = NA.NamedAction (sendMessage msg, describe' msg:[]) where
    describe' (Go d2)       = "WinNav: Go "       <> show d2
    describe' (Swap d2)     = "WinNav: Swap "     <> show d2
    describe' (Move d2)     = "WinNav: Move "     <> show d2
    describe' (Apply _f d2) = "WinNav: Apply f? " <> show d2

  cmdEnum _ = [f d2 | d2 <- enumConstrs, f <- [Go, Swap, Move]]

instance Typeable a => IsCmd (Toggle a) where
  command msg = NA.NamedAction (sendMessage msg, describe' msg:[]) where
    describe' (Toggle t) = "Toggle " <> show (typeOf t)
  cmdEnum _ = [] -- TODO upside down

instance Show MagnifyMsg where
  show Magnifier.MagnifyLess = "Magnify (less)"
  show Magnifier.MagnifyMore = "Magnify (more)"
  show Magnifier.Toggle      = "Magnify (toggle)"
  show Magnifier.ToggleOff   = "Magnify (off)"
  show Magnifier.ToggleOn    = "Magnify (on)"

deriving instance Show BSP.Rotate
deriving instance Show BSP.Swap
deriving instance Show BSP.ResizeDirectional
deriving instance Show BSP.TreeRotate
deriving instance Show BSP.TreeBalance
deriving instance Show BSP.FocusParent
deriving instance Show BSP.SelectMoveNode
deriving instance Show BSP.SplitShiftDirectional

deriving instance Data MagnifyMsg
deriving instance Data ChangeLayout
deriving instance Data Resize
deriving instance Data IncMasterN
deriving instance Data ToggleStruts
deriving instance Data XP.Direction1D
deriving instance Data BSP.Direction2D
deriving instance Data BSP.Rotate
deriving instance Data BSP.Swap
deriving instance Data BSP.ResizeDirectional
deriving instance Data BSP.TreeRotate
deriving instance Data BSP.TreeBalance
deriving instance Data BSP.FocusParent
deriving instance Data BSP.SelectMoveNode
deriving instance Data BSP.SplitShiftDirectional
