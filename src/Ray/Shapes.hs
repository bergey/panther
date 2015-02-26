{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

-- | Geometric objects and their instances.

module Ray.Shapes where

import Ray.Types
import Ray.Imports
import Solve

import qualified Numeric.Interval as I

import Data.Vector (Vector, (!))

class Intersectable a where
    type IntersectionData a :: *
    intersect :: a -> Ray -> Option (Intersection (IntersectionData a))

instance Intersectable Object where
    type IntersectionData Object = Material
    intersect (Object shape mat) ray = intersect shape ray
                                       & _Wrapped . _Just . material .~ mat

onRaySegment :: Ray -> Double -> Bool
onRaySegment ray d = I.elem d (ray ^. rayt)

-- | A multiplier of 'intersection' tHit' to calculate ''tEpsilon'
-- PBRT v2 p. 123 suggests the value as effective in practice
epsilonFactor :: Double
epsilonFactor = 5e-4

instance Intersectable Shape where
      type IntersectionData Shape = ()
      intersect (Sphere center radius) ray =
        -- http://www.siggraph.org/education/materials/HyperGraph/raytrace/rtinter1.htm
        case filter (onRaySegment ray) (quadForm a b c) of
         [] -> Option Nothing
         roots -> let
             dist = minimum roots
             pt = o .+^ (dist *^ dir)
             in Option . Just $ Intersection dist (epsilonFactor * dist) (pt .-. center) ()
        where
          a = quadrance dir
          b = 2 * dir `dot` (o .-. center)
          c = quadrance (o .-. center) - radius * radius
          o = ray ^. rayOrigin
          dir = ray ^. rayDir
      intersect (Plane nrm d) ray =
        let
            p = ray ^. rayOrigin . _Point
            t = (d - dot p nrm)  / dot (ray ^. rayDir) nrm
        in if onRaySegment ray t then
            Option . Just $ Intersection {
                _tHit = t,
                _tEpsilon = epsilonFactor * t,
                _normal = nrm,
                _material = ()
                }
        else Option Nothing
      intersect (Mesh ps vis ns _ts _uvs) ray =
          foldMap (intersectTriangle ps ns ray) vis

-- Algorithm from PBRTv2 p. 140-145
intersectTriangle :: Vector P3D -> Maybe (Vector V3D) -> Ray -> V3 Int
                     -> Option (Intersection ())
intersectTriangle ps ns ray ixs =
    if divisor /= 0
       && I.elem b1 (0...1)
       && I.elem b2 (0...1)
       && I.elem t (ray ^. rayt)
    then Option . Just $ Intersection t ε nrm ()
    else Option Nothing
  where
    V3 p1 p2 p3 = fmap (ps !) ixs
    s = ray ^. rayOrigin .-. p1
    e1 = p2 .-. p1
    e2 = p3 .-. p1
    d = ray ^. rayDir
    s1 = d `cross` e2
    s2 = d `cross` e1
    divisor = dot s1 e1
    invDivisor = 1 / divisor
    b1 = dot s s1 * invDivisor
    b2 = dot d s2 * invDivisor
    t = dot e2 s2 * invDivisor
    nrm = case ns of
        Nothing -> -- pick normal from vertices
            e1 `cross` e2
        Just ns' -> -- interpolate shading normals
            getSum $ foldMap Sum weighted where
              barycentric :: V3D
              barycentric = V3 b1 b2 (1 - b1 - b2)
              normals :: V3 V3D
              normals = (ns' !) <$> ixs
              weighted :: V3 V3D
              weighted =  (*^) <$> barycentric <*> normals
    ε = 1e-3 * t

instance Intersectable a => Intersectable [a] where
    type IntersectionData [a] = IntersectionData a
    intersect os ray =
        case catMaybes $ getOption . flip intersect ray <$> os of
        [] -> Option Nothing
        is -> Option . Just $ minimumBy (comparing _tHit) is

instance Intersectable Scene where
    type IntersectionData Scene = Material
    intersect = intersect . _visibles
