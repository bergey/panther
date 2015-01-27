-- | Geometric objects and their instances.

module Ray.Shapes where

import Ray.Types
import Solve

import Linear
import Linear.Affine

import Prelude (Double, Int, realToFrac, IO, ($), Real, Fractional, filter, Num(..), (.))
-- import Control.Monad.Identity
-- import Control.Monad.Reader
import Data.Maybe
import Data.Array
import Data.Traversable
import Data.Distributive
import Data.Ord
import Data.Foldable
import Control.Applicative

class Intersectable a where
    intersect :: a -> IntersectionTest

instance Intersectable Object where
    intersect (Object (Sphere center radius) reflectance) (Ray from dir _) =
        -- http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtinter1.htm
        case filter (>0) (quadForm a b c) of
         [] -> Nothing
         roots -> let
             dist = minimum roots
             pt = from .+^ (dist *^ dir)
             in Just $ Intersection dist (pt .-. center) reflectance
      where
        a = quadrance dir
        b = 2 * dir `dot` (from .-. center)
        c = quadrance (from .-. center) - radius

instance Intersectable a => Intersectable [a] where
    intersect os ray = case catMaybes . distribute (intersect <$> os) $ ray of
        [] -> Nothing
        is -> Just $ minimumBy (comparing _distance) is

instance Intersectable Scene where
    intersect = intersect . _visibles
