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
import Data.Bool

class Intersectable a where
    type IntersectionData a :: *
    intersect :: a -> Ray -> Maybe (Intersection (IntersectionData a))

instance Intersectable Object where
    type IntersectionData Object = Material
    intersect (Object shape mat) ray = intersect shape ray
                                       & _Just . material .~ mat

onRaySegment :: Ray -> Double -> Bool
onRaySegment ray d = d > (ray ^. mint) && d < (ray ^. maxt)

-- | A multiplier of 'intersection' tHit' to calculate ''tEpsilon'
-- PBRT v2 p. 123 suggests the value as effective in practice
epsilonFactor :: Double
epsilonFactor = 5e-4

instance Intersectable Sphere where
      type IntersectionData Sphere = ()
      intersect (Sphere center radius) ray =
        -- http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtinter1.htm
        case filter (onRaySegment ray) (quadForm a b c) of
         [] -> Nothing
         roots -> let
             dist = minimum roots
             pt = from .+^ (dist *^ dir)
             in Just $ Intersection dist (epsilonFactor * dist) (pt .-. center) ()
        where
          a = quadrance dir
          b = 2 * dir `dot` (from .-. center)
          c = quadrance (from .-. center) - radius * radius
          from = ray ^. rayOrigin
          dir = ray ^. rayDir

instance Intersectable Plane where
    type IntersectionData Plane = ()
    intersect (Plane normal pDist) ray =
        let t = (- (dot (ray ^. rayOrigin . _Point) normal + pDist / dot (ray ^. rayDir) normal)) in
        if t > ray ^. mint then
            Just $ Intersection {
                _tHit = t,
                _tEpsilon = epsilonFactor * t,
                _normal = normal,
                _material = ()
                }
        else Nothing

instance Intersectable Shape where
    type IntersectionData Shape = ()
    intersect (SSphere s) = intersect s
    intersect (SPlane p) = intersect p

instance Intersectable a => Intersectable [a] where
    type IntersectionData [a] = IntersectionData a
    intersect os ray = case catMaybes . distribute (intersect <$> os) $ ray of
        [] -> Nothing
        is -> Just $ minimumBy (comparing _tHit) is

instance Intersectable Scene where
    type IntersectionData Scene = Material
    intersect = intersect . _visibles
