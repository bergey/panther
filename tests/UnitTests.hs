{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

-- | Unit Tests

module Main where

import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Test.Tasty

import Types -- Arbitrary instances
import Ray
import Ray.Imports

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Panther Tests" [
    testGroup "sphere" [
         testProperty "intersect sphere at origin with radius 1" $
         \dir -> dir /= 0 ==>
                 intersect (Sphere 0 1) (mkRay 0 (normalize dir) 0) ^? _Wrapped . _Just . tHit =~ Just 1,
         testProperty "sphere has fixed r" $
         \(getSphere -> s@(Sphere c r), dir) -> dir /= 0 && r > 0 ==>
                                   intersect s (mkRay c (normalize dir) 0) ^? _Wrapped . _Just . tHit =~ Just r
         ],
    testGroup "plane" [
        testProperty "all rays intersect a plane" $
        \(getPlane -> plane, ray) -> isJust (getOption $ intersect (plane) ray)
                          || isJust (getOption $ intersect plane $ ray & rayDir *~ -1)
                      ],
    testGroup "mesh" [
        testCase "a ray intersects a triangle" $ let
             mesh = oneTriangleMesh $ V3 0 (p3 1 0 0) (p3 0 1 0)
             ray = mkRay (p3 0.33 0.33 1) (V3 0 0 (-1)) 0
        in intersect mesh ray ^? _Wrapped . _Just . tHit @?= Just 1,
        testProperty "arbitrary direction, no false negatives, correct tHit" $
        \(tri, bary, v) -> let
            ray = mkRay o v 0
            o = barycentric (firstTriangle tri) bary .-^ v
            in intersect (getTri tri) ray ^? _Wrapped . _Just . tHit =~ Just 1,
        testProperty "arbitrary direction, no false positives" $
        \(tri, bary, v) -> let
            ray = mkRay o v 0
            o = barycentric (firstTriangle tri) bary .+^ v
            in intersect (getTri tri) ray ^? _Wrapped . _Just . tHit =~ Nothing

        -- testProperty "arbitrary rays, no false positives" $
        -- \(tri, bary, v) -> let
        ]
    ]


------------------------------------------------------------
    -- Approximate Comparison for Doubles, Points

ε :: Floating a => a
ε = 0.0001

class Approx a where
  (=~) :: a -> a -> Bool

infix 4 =~

instance Approx Double where
  (=~) a b = abs (a - b) < ε

-- instance Epsilon a => Approx a where
--     a =~ b = nearZero $ a - b

-- instance (Metric f, Floating a, Ord a) => Approx (f a) where
--     u =~ v = distance u v < ε
instance (Floating a, Ord a) => Approx (Point V3 a) where
    u =~ v = distance u v < ε

instance Approx a => Approx (Maybe a) where
    Nothing =~ Nothing = True
    Just a =~ Just b = a =~ b
    _ =~ _ = False
