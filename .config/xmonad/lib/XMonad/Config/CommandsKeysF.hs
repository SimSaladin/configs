{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -Wno-orphans #-}

------------------------------------------------------------------------------
-- |
-- Module      : XMonad.Config.CommandsKeysF
-- Description : Short description
-- Copyright   : (c) Samuli Thomasson, 2019
-- License     : BSD-3
--
-- Maintainer  : Samuli Thomasson <samuli.thomasson@paivola.fi>
-- Stability   : unstable
-- Portability : unportable
--
------------------------------------------------------------------------------

module XMonad.Config.CommandsKeysF
  (
   -- * Usage
   -- $usage

   module XMonad.Config.CommandsKeysF -- TODO
  ) where

import           XMonad
import qualified XMonad.Util.EZConfig      as EZ
import           XMonad.Util.NamedActions  (NamedAction)
import qualified XMonad.Util.NamedActions  as NA
import qualified XMonad.Util.ExtensibleState         as XS
import qualified XMonad.Prompt as XP

import           XMonad.Util.NamedCommands

import           Control.Applicative
import           Control.Monad.Fix
import           Control.Monad.Free
import           Control.Arrow (second,(***))
import           Data.Char                 (isSpace)
import           Data.Either               (lefts)
import           Data.Foldable
import           Data.Function             (on)
import           Data.List                 (union)
import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Tree
import           Data.Unique
import           Prelude
import           Text.Printf

-- $usage
--
--      KeyMask ~ ButtonMask ~ Modifier ~ CUInt
--      KeySym ~ XID ~ Word64
--
--      keysymToString  :: KeySym -> String
--      keysymToKeycode :: Display -> KeySym -> IO KeyCode
--
--  XMonad.Util.EZConfig:
--      parseKey :: ReadP

type Cmd l a = Free (CmdF l) a

data CmdF l a
  = Group GroupName (Cmd l ()) a
  | Command CmdId SomeCmd a
  | CommandW CmdId (Window -> SomeCmd) a
  | BindKey EZKey CmdId a
  | BindButton ButtonMask Button CmdId a
  | MkId (CmdId -> a)
  | WithXConfig (XConfig l -> a)
  | CurrentModMask (KeyMask -> a)
  | WithHelp Int (X () -> a)
  deriving (Functor)

type GroupName = String
type EZKey     = String
type ShowKeys  = [GroupName] -> [((KeyMask,KeySym),NamedAction)] -> X ()
type SomeCmdW  = Window -> SomeCmd
type CmdId     = Int

instance Show SomeCmdW where
  show f = show (f 1)

-- * Interpret

data Builder = Builder
  { bKeys       :: !(Forest (Either GroupName (EZKey, CmdId)))
  , bButtons    :: !(Map (ButtonMask, Button) CmdId)
  , bCommands   :: !(Map CmdId SomeCmd)
  , bCommandsW  :: !(Map CmdId SomeCmdW)
  } deriving (Show)

instance Semigroup Builder where
  a <> b = Builder
    { bKeys      = (mappend `on` bKeys) a b
    , bButtons   = (mappend `on` bButtons) a b
    , bCommands  = (mappend `on` bCommands) a b
    , bCommandsW = (mappend `on` bCommandsW) a b
    }

instance Monoid Builder where
  mempty = Builder mempty mempty mempty mempty

instance ExtensionClass Builder where
  initialValue = mempty

interp :: forall l. ShowKeys -> XConfig l -> Cmd l () -> IO Builder
interp sk xc = interp'
  where
    interp' :: Cmd l () -> IO Builder
    interp' = iterM alg . fmap (\() -> mempty)

    alg :: CmdF l (IO Builder) -> IO Builder
    alg (MkId f)              = newUnique >>= f . hashUnique
    alg (CurrentModMask f)    = f (modMask xc)
    alg (WithXConfig f)       = f xc
    alg (WithHelp d f)        = mfix $ \r -> f (sk [] (toKeys d sk [(g,(k,c))|(g,(k,c))<-keysList r] xc))
    alg (Group name gx x)     = interp' gx >>= \r -> mappend r{bKeys = Node (Left name) (bKeys r) : []} <$> x
    alg (BindKey k ix x)      = mappend mempty{bKeys      = Node (Right (k,ix)) [] : []} <$> x
    alg (BindButton m b ix x) = mappend mempty{bButtons   = M.singleton (m,b) ix} <$> x
    alg (Command i c x)       = mappend mempty{bCommands  = M.singleton i c} <$> x
    alg (CommandW i c x)      = mappend mempty{bCommandsW = M.singleton i c} <$> x

addAll :: ShowKeys -> Cmd l () -> XConfig l -> IO (XConfig l)
addAll sk cmds xc = do
  builder@Builder{..} <- interp sk xc cmds
  return $ xc { keys = \_ -> mempty }
    `EZ.additionalKeys` [(k,NA.getAction v) | (k,v) <- toKeys maxBound sk (keysList builder) xc]
    `EZ.additionalMouseBindings` [(b,NA.getAction . command . c) | (b,ix) <- M.toList bButtons, Just c <- [M.lookup ix bCommandsW]]
    `saveState` builder
  where
    saveState xc st = xc { startupHook = startupHook xc >> XS.put st }

keysList :: Builder -> [([GroupName],(EZKey,SomeCmd))]
keysList Builder{..} = go [] bKeys
  where go g (Node (Left x) xs      : xss) = go (g ++ [x]) xs ++ go g xss
        go g (Node (Right (k,ix)) _ : xss) = [(g,(k,a))|Just a<-[M.lookup ix bCommands]] ++ go g xss
        go _                            [] = []

toKeys :: Int -> ShowKeys -> [([GroupName],(EZKey,SomeCmd))] -> XConfig l -> [((KeyMask,KeySym),NamedAction)]
toKeys depth showKeys klist xc = go depth [] [(k,(g,command c))|(g,(k,c))<-klist]
  where
    go 0 _    = const []
    go d subk = EZ.mkNamedKeymap xc . map (go1 d subk) . M.toList . M.fromListWith f . map go'
      where
        go' (k,(g,a)) = (k',([g],res))
          where (k',ks)         = span (not . isSpace) (dropWhile isSpace k)
                ks'             = dropWhile isSpace ks
                res | null ks'  = Right a
                    | d == 1    = Right (return () ? printf "Submap (%s)" (unwords g))
                    | otherwise = Left [(ks',(g,a))]

    go1 d subk (k,(_,r)) = (k,either (smap (subk ++ [k]) . go (d-1) (subk ++ [k])) id r)

    smap subk km = NA.submapDefaultName (showKeys subk km ? ("Submap: " ++ unwords subk)) km

    f (g,r) (g',r') = (union g g', r' <> r <> Left (concat $ lefts [r,r']))

-- * The configuration EDSL
--
-- Used to define customized commands (X actions).
--
-- Most Message types are small, e.g. N-ary sums, either without any arguments or some (Bounded,Enum) arguments (1D and 2D-navigatian directions for instance). So oftentimes the autogeneraed command options are quite sensible.
-- Important exception to the above are arguments infinite or near-finite arguments (numerals nad string literals mostly). For now, it's easiest to select a few discete value sets you're going to use and generate  commands for each of those
--
-- n numbered lists rec.
-- j remove a commend leader when joining lines, if it mould make sense
-- c auto-wrap commands  using &tw inserting commant leader automatically
-- r auto-insert comment header after hitting <enter> in insert buf
-- o auto-insert coomment leader ater hitting o or O in normal mode
-- q* allow formatting of comments with gf program
-- w treat trailing whitespace as "line continues below"
-- a automatically format paragraphs
-- q Allow formatting commens with gq

mod :: KeyMask -> Cmd l () -> Cmd l ()
mod modm x = undefined modm x

modDef :: (KeyMask -> Cmd l ()) -> Cmd l ()
modDef f = liftF (CurrentModMask id) >>= f

group :: GroupName -> Cmd l () -> Cmd l ()
group nm x = liftF (Group nm x ())

key :: IsCmd cmd => EZKey -> cmd -> Cmd l CmdId
key k cmd = do
  ix <- liftF (MkId id)
  liftF (Command ix (toCmd cmd) ())
  liftF (BindKey k ix ix)

key' :: EZKey -> CmdId -> Cmd l ()
key' k ix = liftF (BindKey k ix ())

btn :: (IsCmd cmd) => ButtonMask -> Button -> (Window -> cmd) -> Cmd l ()
btn m b cmd = do
  ix <- liftF (MkId id)
  liftF (CommandW ix (toCmd . cmd) ())
  liftF (BindButton m b ix ())

-- | @ hinted "Title" submapDepth $ \displayHints -> ... @
hinted :: String -> Int -> (CmdId -> Cmd l ()) -> Cmd l ()
hinted s depth f = do
  ix <- liftF (MkId id)
  x  <- liftF (WithHelp depth id)
  liftF (Command ix (toCmd (x ? s)) ())
  f ix

withXConfig :: (XConfig l -> a) -> Cmd l a
withXConfig f = liftF (WithXConfig f)

-- ** Helpers

-- infixr 9 .
-- infixr 0 $
-- infixl 1 >>
-- infixl 1 >>=

infixr 0 >+
infixr 1 >>+
infixr 1 >++>

-- | Define command bond to a key.
-- @
-- "M-r" >+ promptCommand myXPConfig
-- @
--
-- Same as 'key'.
(>+) :: IsCmd a => EZKey -> a -> Cmd l ()
(>+) k cmd = key k cmd >> return ()

-- Same as 'btn'.
(/+) :: IsCmd a => (ButtonMask, Button) -> (Window -> a) -> Cmd l ()
(/+) = uncurry btn

(>>+) :: (Traversable t, IsCmd a) => EZKey -> t (EZKey, a) -> Cmd l ()
(>>+) mod xs = (mod `keysOf'` xs) id

(>++>) :: Traversable t => t (EZKey, a) -> (a -> b) -> t (EZKey, b)
(>++>) xs = (`fmap` xs) . second

-- | Compose actions easily with a list of arguments and bindings (say, @[("h",True),("l",False)]@)
-- and a function (say, @Bool -> NamedAction)@.
--
-- The first argument is the key prefix, it will be prepended to every binding. Example:
-- @
-- keysOf' "M-b S-" [("h",True),("l",False)] (\b -> windows (if b then focusUp else focusDown))
-- @
keysOf' :: (Traversable t, IsCmd b) => EZKey -> t (EZKey, a) -> (a -> b) -> Cmd l ()
keysOf' mod xs f = sequenceA_ [ key (mod <> m) (f x) | (m,x) <- toList xs]

-- | Like keysOf', but takes an extra parameter to use as the name.
keysOf :: (Traversable t, PrintfArg a) => EZKey -> t (EZKey, a) -> String -> (a -> X ()) -> Cmd l ()
keysOf mod xs desc f = keysOf' mod xs (f .? printf "%s (%s)" desc)

-- * XPrompts to view, choose, perform commands                    {{{1

promptCommand :: XP.XPConfig -> NamedAction
promptCommand xpc = prompt ? "Prompt (cmd: any)" where
  prompt = do xs <- XS.gets bCommands
              mkCmdPrompt (CmdPrompt "Command" (M.elems xs)) xpc
