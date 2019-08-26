{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE LambdaCase                #-}
{-# OPTIONS_GHC
   -Weverything
   -Wno-partial-type-signatures
   -Wno-missing-import-lists
   -Wno-unused-top-binds
   -Wno-unused-do-bind
  #-}


module Two (..) where

-- |
-- Module         : Main
-- Maintainer     : Samuli Thomasson <samuli.thomasson@relexsolutions.com>
-- Stability      : experimental
-- Portability    : non-portable
module Three (
  module B,
  module C,

  -- * A section heading
  main,
  foo, bar

  -- $doc
 ) where

import           Prelude

import qualified Control.Exception as Exception (bracket, try)
import           Control.Monad     (filterM, forM, forM_, guard, join, liftM2,
                                    unless, when, (>=>))
import           Prelude

main :: IO ()

-- * H1
-- ** H2
-- *** H3

-- $namedsection
-- blah blah blah

f :: [String]
f = _


-- * H1 (2)

-- | T data type
data T a b
  -- | This is the documentation for the 'C1' constructor
  = C1 a b
  -- | This is the documentation for the 'C2' constructor
  | C2 a b

-- | R data type
-- second line
-- blah blah
data R a b =
  C { -- | This is the documentation for the 'a' field
      -- blaah blaah
      a :: a,
      -- | This is the documentation for the 'b' field
      b :: b
    }

data R a b =
  C { a :: a  -- ^ This is the documentation for the 'a' field
    , b :: b  -- ^ This is the documentation for the 'b' field
    }

f  :: Int      -- ^ The 'Int' argument
   -> Float    -- ^ The 'Float' argument
   -> IO ()    -- ^ The return value

-- $doc
-- Here is a large chunk of documentation which may be referred to by
-- the name $doc.

-- vim:foldcolumn=5:

module One where

f :: alreuch
f _ = _

data hellowROld
data Hellooauelrch
