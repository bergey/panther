{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Unit Tests

module Main where

import Test.Tasty.QuickCheck
import Test.Tasty

import Ray
import Linear
import Linear.Affine

import Control.Applicative
import Control.Lens
import Data.Maybe

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Panther Tests" [
    testGroup "sphere" [
         testProperty "intersect sphere at origin with radius 1" $
         \dir -> dir /= 0 ==>
                 intersect (Sphere 0 1) (mkRay 0 (normalize dir) 0) ^?! _Just . tHit =~ 1,
         testProperty "sphere has fixed r" $
         \(s@(Sphere c r), dir) -> dir /= 0 && r > 0 ==>
                                   intersect s (mkRay c (normalize dir) 0) ^?! _Just . tHit =~ r
         ],
    testGroup "plane" [
        testProperty "all rays intersect a plane" $
        \(plane, ray) -> isJust (intersect (plane :: Plane) ray)
                          || isJust (intersect plane $ ray & rayDir *~ -1)
                      ]
    ]

nonZero :: (Num a, Ord a, Arbitrary a) => Gen a
nonZero = getNonZero <$> arbitrary

instance (Num a, Ord a, Arbitrary a) => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

instance (Num a, Ord a, Arbitrary a) => Arbitrary (V3 a) where
    arbitrary = V3 <$> nonZero <*> nonZero <*> nonZero

instance (Num a, Ord a, Arbitrary a) => Arbitrary (V4 a) where
    arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Num a, Ord a, Arbitrary a, Arbitrary (v a)) => Arbitrary (Point v a)
                                                                                where
    arbitrary = P <$> arbitrary

instance Arbitrary Ray where
    arbitrary = mkRay <$> arbitrary <*> (getNonZero <$> arbitrary) <*> arbitrary

instance Arbitrary Sphere where
    arbitrary = Sphere <$> arbitrary <*> arbitrary

instance Arbitrary Plane where
    arbitrary = Plane <$> arbitrary <*> arbitrary

instance Arbitrary Shape where
    arbitrary = oneof [ SSphere <$> arbitrary, SPlane <$> arbitrary ]

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

instance (Metric f, Floating a, Ord a) => Approx (f a) where
    u =~ v = distance u v < ε
