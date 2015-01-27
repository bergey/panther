{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Mutually recursive data types.

module Ray.Types where

import           Linear
import           Linear.Affine
import           Linear.Projection

import Prelude (Double, Int, realToFrac, IO, ($), Real, Fractional, Show)
import Control.Lens.TH
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Maybe
import Data.Array
import Data.Traversable
import Data.Distributive
import Data.Ord
import Data.Foldable
import Control.Applicative

type V3D = V3 Double
type P3D = Point V3 Double
type Dir = V3D

type Spectrum = Double

-- TODO is it easier to require normalized direction vector?
-- Certainly it doesn't change the denotation of the type.

-- | A ray is represented as a starting point and a vector.  The
-- vector need not be normalized.
data Ray = Ray !P3D !V3D !Spectrum deriving Show

-- XXX Will this be true?
-- A Ray carries an arbitrary tag, typically representing the light
-- (if tracing from lamp to eye) or the accumulated absorption (if
-- tracing from eye to lamp).

data Shape = Sphere !P3D !Double deriving Show

-- TODO check definition of Reflectance, Reflectivity, &c.
type Material = Double -- ^ diffuse Reflectance

data Object = Object !Shape !Material deriving Show

-- | Normal & material are not strict fields, since they may not be used.
data Intersection = Intersection {
    _distance :: !Double,
    _normal :: Dir,
    _material :: Material
    }

type IntersectionTest = Ray -> Maybe Intersection


data Light = PointLight !P3D !Double -- ^ brightness
           deriving Show

-- | follows Haines's NFF: http://tog.acm.org/resources/SPD/NFF.TXT
data Camera = Camera {
    _eye :: !P3D,
    _lookAtPt :: !P3D,
    _upDir :: !V3D, -- ^ calculations assume this is perpendicular to eye .-. lookAt, normalized
    _fov :: !Double, -- ^ y direction, in radians
    _nearPlane :: !Double -- ^ distance to image plane
    } deriving Show

data Scene = Scene {
    _camera :: Camera,
    _background :: Spectrum,
    _lights :: [Light],
    _visibles :: [Object]
    } deriving Show

r2f :: (Real a, Fractional b) => a -> b
r2f = realToFrac

data Algo = Algo {
    _samplesPerPixel :: Int,
    _samplesPerCameraRay :: Int,
    _resolution :: V2 Int,
    _imageSampler :: ImageSampler,
    _surfaceIntegrator :: SurfaceIntegrator,
    _imageReconstructor :: ImageReconstructor
    }

data MRead = MRead {
    _algorithms :: Algo,
    _scene :: Scene
    }

type M = ReaderT MRead Identity --(StateT s Identity)

runM :: M a -> Algo -> Scene -> IO a
runM ma a s = return $ runReader ma (MRead a s)

-- | Given the number of samples per pixel, and the image resolution,
-- return a set of sample coordinates, on the image plane.
type ImageSampler = Int -> V2 Int -> M [V2 Double]

-- type Sampler1D = Int -> M [Double]
-- type Sampler2D = Int -> M [V2D]

type DirectionSampler = Int -> M [Dir]

type SurfaceIntegrator = Int -> Ray -> Intersection -> M Spectrum

data  ImgSample a = ImgSample {
    _sampleLocation :: !(V2 Double),
    _sampleValue :: !a
    } deriving (Functor, Foldable, Traversable, Show)

type ImageReconstructor =
    Array (V2 Int) [ImgSample Spectrum] -> Array (V2 Int) Spectrum
    -- V2 Int -> [ImageSample Spectrum] -> Spectrum

makeLenses ''Algo
makeLenses ''Camera
makeLenses ''MRead
makeLenses ''Scene
makeLenses ''ImgSample
