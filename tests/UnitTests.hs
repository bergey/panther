{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
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
import Data.Semigroup

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Panther Tests" [
    testGroup "sphere" [
         testProperty "intersect sphere at origin with radius 1" $
         \dir -> dir /= 0 ==>
                 intersect (Sphere 0 1) (mkRay 0 (normalize dir) 0) ^?! _Wrapped . _Just . tHit =~ 1,
         testProperty "sphere has fixed r" $
         \(getSphere -> s@(Sphere c r), dir) -> dir /= 0 && r > 0 ==>
                                   intersect s (mkRay c (normalize dir) 0) ^?! _Wrapped . _Just . tHit =~ r
         ],
    testGroup "plane" [
        testProperty "all rays intersect a plane" $
        \(getPlane -> plane, ray) -> isJust (getOption $ intersect (plane) ray)
                          || isJust (getOption $ intersect plane $ ray & rayDir *~ -1)
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

data Sphere = SSphere Shape
              deriving (Show)

getSphere :: Sphere -> Shape
getSphere (SSphere s) = s

instance Arbitrary Sphere where
    arbitrary = SSphere <$> (Sphere <$> arbitrary <*> arbitrary)

data Plane = SPlane Shape
              deriving (Show)

getPlane :: Plane -> Shape
getPlane (SPlane s) = s

instance Arbitrary Plane where
    arbitrary = SPlane <$> (Plane <$> arbitrary <*> arbitrary)

instance Arbitrary Shape where
    arbitrary = oneof [ getSphere <$> arbitrary, getPlane <$> arbitrary ]

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
