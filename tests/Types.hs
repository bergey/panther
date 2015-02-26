{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Arbitrary instances, and wrappers around types from Ray.Types,
-- for more useful Arbitrary instances.

module Types where

import qualified Data.Vector as V
import           Ray.Imports
import           Ray.Types
import           Test.Tasty.QuickCheck as QC
import System.Random (Random)

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

-- | A Mesh with a single random triangle.
data Tri = STri Shape
         deriving (Show)

getTri :: Tri -> Shape
getTri (STri s) = s

oneTriangleMesh :: Triangle -> Shape
oneTriangleMesh t = simpleMesh (V.fromList . toList $ t) (V.singleton $ V3 0 1 2)

instance Arbitrary Tri where
    arbitrary = STri . oneTriangleMesh <$> arbitrary where
      -- ps = V.fromList <$> QC.vector 3
      -- vis = pure . V.singleton $ V3 0 1 2
      -- no = pure Nothing

-- | A single triangle.
type Triangle = V3 (P3D)

firstTriangle :: Tri -> Triangle
firstTriangle (STri (Mesh ps vis _ _ _)) = (ps V.!) <$> (V.head vis)
firstTriangle _ = error "Somehow we constructed a Triangle newtype that isn't a triangle mesh"

normalToTriangle :: Triangle -> V3D
normalToTriangle (V3 p1 p2 p3) = e1 `cross` e2 where
  e1 = p2 .-. p1
  e2 = p3 .-. p1

-- | The closed unit intervalp
unitInterval :: (Num a,  Random a) => Gen a
unitInterval = choose (0,1)

newtype Barycentric a = Barycentric (V3 a)
                      deriving (Show)

getBary :: Barycentric a -> V3 a
getBary (Barycentric b) = b

instance (Num a, Random a) => Arbitrary (Barycentric a) where
    arbitrary = do
        x <- unitInterval
        y <- choose (0, 1 - x)
        let z = 1 - x - y
        return . Barycentric $ V3 x y z
        -- Barycentric <$> (V3 <$> unitInterval <*> unitInterval <*> unitInterval)

barycentric :: (Functor f, Num (f a), Num a) => V3 (f a) -> Barycentric a -> f a
barycentric ps (Barycentric coords) = sum weighted where
  weighted = (*^) <$> coords <*> ps

newtype OutOfGamut a = OutOfGamut (V3 a)
                     deriving (Show)

getOoG :: OutOfGamut a -> V3 a
getOoG (OutOfGamut b) = b

instance (Num a, Random a, Arbitrary a, Ord a) => Arbitrary (OutOfGamut a) where
    arbitrary = OutOfGamut <$> arbitrary `suchThat` \u ->
          maximum u > 1
