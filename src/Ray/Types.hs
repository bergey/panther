{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Mutually recursive data types.

module Ray.Types where

import Ray.Imports
import Ray.Util

import Control.Monad.Reader
import Data.Array
import Data.Vector (Vector)
import System.Random.MWC

type V3D = V3 Double
type P3D = Point V3 Double
type Dir = V3D

-- | A linear RGB space, assumed to use the BT.709 primaries
-- (equivalent to sRGB, but linear.)
type Spectrum = V3D

-- | By convention, components of RGB should be in [0,1].
type RGB = V3D

-- TODO is it easier to require normalized direction vector?
-- Certainly it doesn't change the denotation of the type.

-- | A ray is represented as a starting point and a vector.  The
-- vector need not be normalized.
data Ray = Ray {
    _rayOrigin :: !P3D,
    _rayDir :: !V3D,
    _rayt :: !(Interval Double),
    _raySpectrum :: !Spectrum
    } deriving Show

-- | A ray defined for t ∈ 0...∞.
mkRay :: P3D -> V3D -> Spectrum -> Ray
mkRay p u s = Ray p u nonNegative s

-- XXX Will this be true?
-- A Ray carries an arbitrary tag, typically representing the light
-- (if tracing from lamp to eye) or the accumulated absorption (if
-- tracing from eye to lamp).

-- | Solid geometry, without any description of optical behavior.  In
-- the case of Mesh, shading normals are included, as quasi-geometry.
-- Mesh is the most important constructor here; Sphere and Plane are
-- provided to help in debugging other parts of the system.
data Shape = Sphere !P3D !Double
           | Plane !V3D !Double
             -- points, vertex indices, normals, tangents, texture parameters
           | Mesh !(Vector P3D) !(Vector (V3 Int)) (Maybe (Vector V3D)) (Maybe (Vector V3D)) (Maybe (Vector (V2 Double)))
           deriving Show

-- | Construct a Mesh from vertex coordinates and indices, without any
-- shading geometry.
simpleMesh :: Vector P3D -> Vector (V3 Int) -> Shape
simpleMesh ps vis = Mesh ps vis Nothing Nothing Nothing

-- TODO check definition of Reflectance, Reflectivity, &c.
type Material = V3D -- ^ diffuse Reflectance

data Object = Object !Shape !Material deriving Show

-- | Normal & material are not strict fields, since they may not be
-- used.
data Intersection a = Intersection {
    _tHit :: !Double,
    _tEpsilon :: !Double,
    _normal :: Dir,
    _material :: a
    } deriving (Functor, Show)

-- The Semigroup instance picks the *closest* intersection
instance Semigroup (Intersection a) where
    a <> b = if _tHit a <= _tHit b then a else b

data Lamp = PointLamp !P3D !Spectrum
           | ParallelLamp !V3D !Spectrum
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
    _lamps :: [Lamp],
    _visibles :: [Object]
    } deriving Show

r2f :: (Real a, Fractional b) => a -> b
r2f = realToFrac

data Algo = Algo {
    _samplesPerPixel :: Int,
    _samplesPerCameraRay :: Int,
    _resolution :: V2 Int,
    _imageSampler :: ImageSampler,
    _discreteSampler :: DiscreteSampler,
    _surfaceIntegrator :: SurfaceIntegrator,
    _imageReconstructor :: ImageReconstructor,
    _toneMapping :: ToneMapping
    }

data MRead = MRead {
    _algorithms :: Algo,
    _scene :: Scene,
    _gen :: GenIO
    }

type M = ReaderT MRead IO

runM :: M a -> Algo -> Scene -> IO a
runM ma a s = do
    gen <- create
    runReaderT ma (MRead a s gen)

runMSystem :: M a -> Algo -> Scene -> IO a
runMSystem ma a s = do
    gen <- createSystemRandom
    runReaderT ma (MRead a s gen)

-- | Given the number of samples per pixel, and the image resolution,
-- return a set of sample coordinates, on the image plane.
type ImageSampler = Int -> V2 Int -> M [V2 Double]

-- type Sampler1D = Int -> M [Double]
-- type Sampler2D = Int -> M [V2D]

-- | sample range, number of samples.  Returned values should be in
-- the range [0,r]
type DiscreteSampler = Int -> Int -> M [Int]

type DirectionSampler = Int -> M [Dir]

type SurfaceIntegrator = Int -> Ray -> Intersection Spectrum -> M Spectrum

data  ImgSample a = ImgSample {
    _sampleLocation :: !(V2 Double),
    _sampleValue :: !a
    } deriving (Functor, Foldable, Traversable, Show)

type Array2D = Array (V2 Int)

type ImageReconstructor =
    Array2D [ImgSample Spectrum] -> Array2D Spectrum
    -- V2 Int -> [ImageSample Spectrum] -> Spectrum

-- | A ToneMapping maps the luminance of an image from [0,∞) to [0,1].
-- It may be non-linear to compensate for the logorithmic perception
-- of the eye.
type ToneMapping =
    Array2D Spectrum -> Array2D V3D

makeLenses ''Algo
makeLenses ''Camera
makeLenses ''MRead
makeLenses ''Scene
makeLenses ''ImgSample
makeLenses ''Intersection
makeLenses ''Ray
