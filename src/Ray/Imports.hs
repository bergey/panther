-- | Common imports which I should expect in scope in every module of
-- this project.  I'd like to spend less time maintaining the
-- qualified import lists, by only having one copy.
--
-- This also makes
-- the module intros a bit easier to read, at the expense of a layer
-- of indirection.

module Ray.Imports
       ( module Prelude
       , module Control.Applicative
       , module Control.Lens
       -- , module Control.Monad
       , module Data.Bool
       , module Data.Distributive
       , module Data.Foldable
       , module Data.Maybe
       , module Data.Ord
       , module Data.Semigroup
       , module Data.Semigroup.Foldable
       , module Data.Traversable

       , module Linear
       , module Linear.Affine
       , module Linear.Projection
       , module Numeric.Interval
       ) where

import Prelude ((!!), ($), (.), (/), (/=), (=<<), (>>=),
                Double, Float, IO, Int, Integer,
                Floating(..), Fractional(..), Num(..), Real(..), Show(..),
                const, filter, flip, fmap, length, realToFrac, return, round, zip,
                error)

import Control.Applicative
import Control.Lens hiding ((...))
-- import Control.Monad
import Data.Bool
import Data.Distributive
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Semigroup
import Data.Semigroup.Foldable
import Data.Traversable

import Linear
import Linear.Affine
import           Linear.Projection
import Numeric.Interval (Interval, (...))

-- Don't import Data.Vector or Data.Array, because they overlap on
-- (!).  Several modules only need one or the other, so don't force
-- qualified usage.
