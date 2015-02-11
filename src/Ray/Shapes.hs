{-# LANGUAGE TypeFamilies #-}

-- | Geometric objects and their instances.

module Ray.Shapes where

import Ray.Types
import Solve

import Linear
import Linear.Affine

import Prelude (Double, Int, realToFrac, IO, ($), Real, Fractional, filter, Num(..), (.), (/), const)
-- import Control.Monad.Identity
-- import Control.Monad.Reader
import Data.Maybe
import Data.Array
import Data.Traversable
import Data.Distributive
import Data.Ord
import Data.Foldable
import Control.Applicative
import Control.Lens

class Intersectable a where
    type IntersectionData a :: *
    intersect :: a -> Ray -> Maybe (Intersection (IntersectionData a))

instance Intersectable Object where
    type IntersectionData Object = Material
    intersect (Object shape mat) ray = intersect shape ray
                                       & _Just . material .~ mat

instance Intersectable Sphere where
      type IntersectionData Sphere = ()
      intersect (Sphere center radius) (Ray from dir _) =
        -- http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtinter1.htm
        case filter (>0) (quadForm a b c) of
         [] -> Nothing
         roots -> let
             dist = minimum roots
             pt = from .+^ (dist *^ dir)
             in Just $ Intersection (dist * dist) (pt .-. center) ()
        where
          a = quadrance dir
          b = 2 * dir `dot` (from .-. center)
          c = quadrance (from .-. center) - radius * radius

instance Intersectable Plane where
    type IntersectionData Plane = ()
    intersect (Plane normal pDist) (Ray (P from) dir _) =
        Just $ Intersection (- (dot from normal + pDist / dot dir normal)) normal ()

instance Intersectable Shape where
    type IntersectionData Shape = ()
    intersect (SSphere s) = intersect s
    intersect (SPlane p) = intersect p

instance Intersectable a => Intersectable [a] where
    type IntersectionData [a] = IntersectionData a
    intersect os ray = case catMaybes . distribute (intersect <$> os) $ ray of
        [] -> Nothing
        is -> Just $ minimumBy (comparing _distanceSq) is

instance Intersectable Scene where
    type IntersectionData Scene = Material
    intersect = intersect . _visibles
