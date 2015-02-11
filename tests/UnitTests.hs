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

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Panther Tests" [
    testProperty "intersect sphere at origin with radius 1" $
    \dir -> dir /= 0 ==>
            intersect (Sphere 0 1) (Ray 0 (normalize dir) 0) ^?! _Just . distanceSq =~ 1,
    testProperty "sphere has fixed r" $
    \(s@(Sphere c r), dir) -> dir /= 0 && r > 0 ==>
                              intersect s (Ray c (normalize dir) 0) ^?! _Just . distanceSq =~ r * r
    ]

instance Arbitrary a => Arbitrary (V2 a) where
    arbitrary = V2 <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V3 a) where
    arbitrary = V3 <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (V4 a) where
    arbitrary = V4 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary (v a)) => Arbitrary (Point v a) where
    arbitrary = P <$> arbitrary

instance Arbitrary Ray where
    arbitrary = Ray <$> arbitrary <*> arbitrary <*> arbitrary

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
