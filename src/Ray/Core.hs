{-# LANGUAGE DeriveFunctor #-}

-- | The main rendering infrastructure, as parameterized by the Algo type.

module Ray.Core where

import Ray.Types
import Ray.Camera
import Ray.Image
import Ray.Shapes
import Ray.Algorithms
import Ray.Util

import Codec.Picture
import Linear
import Linear.Affine
import Data.Array

import Control.Applicative
import Data.Traversable
import Control.Lens

render :: Algo -> Scene -> IO (Image PixelRGBF)
render = runM getImg

renderArray :: Algo -> Scene -> IO (Array (V2 Int) Spectrum)
renderArray = runM getSpectralArray

getImg :: M (Image PixelRGBF)
getImg = asImg <$> view (algorithms . resolution) <*> getSpectralArray

getSpectralArray :: M (Array (V2 Int) Spectrum)
getSpectralArray = do
    recon <- view $ algorithms . imageReconstructor
    recon <$> getSpectralSamples

getSpectralSamples :: M (Array (V2 Int) [ImgSample Spectrum])
getSpectralSamples = overSamples radiance =<< getCameraRays

overSamples :: (a -> M b) ->
               Array (V2 Int) [ImgSample a] -> M (Array (V2 Int) [ImgSample b])
overSamples = traverse . traverse . traverse

-- | A list of pixel coordinates for the given resolution.
enumCoords :: V2 Int -> [V2 Int]
enumCoords = traverse (\r -> [0..r-1])

enumPixelCoords :: M [V2 Int]
enumPixelCoords = enumCoords <$> view (algorithms . resolution)

mkImgSample :: P3D -> V2 Double -> M44 Double -> V2 Double -> ImgSample Ray
mkImgSample o invRes m xy =
    ImgSample xy $ mkRay o (globalRay m (normCoords xy) .-. o) 1
    where normCoords u = 2 * invRes * u - 1

globalRay :: M44 Double -> V2 Double -> P3D
globalRay m u = P $ normalizePoint $ m !* camRay u

camRay :: V2 Double -> V4 Double
camRay (V2 x y) = V4 x (-y) (-1) 1

getCameraRays :: M (Array (V2 Int) [ImgSample Ray])
getCameraRays = do
    m <- getInverseCamMatrix
    ss <- getSampleLocs
    o <- view $ scene . camera . eye
    res <- view (algorithms . resolution)
    let invRes = 1 / (r2f <$> res)
    return $ (fmap . fmap) (mkImgSample o invRes m) ss

getSampleLocs :: M (Array (V2 Int) [V2 Double])
getSampleLocs = do
    sampler <- view $ algorithms . imageSampler
    n <- view $ algorithms . samplesPerPixel
    pxs <- enumPixelCoords
    ss <- traverse (sampler n) pxs
    res <- view $ algorithms . resolution
    return $ array (0, res - 1) $ zip pxs ss

getIntersection :: Ray -> M (Maybe (Intersection Spectrum))
getIntersection r = intersect <$> view scene <*> pure r

radiance :: Ray -> M Spectrum
radiance ray = do
    x <- getIntersection ray
    n <- view $ algorithms . samplesPerCameraRay
    integrator <- view $ algorithms . surfaceIntegrator
    case x of
     Nothing -> view $ scene . background
     Just isect -> (ray ^. raySpectrum * ) <$> integrator n ray isect

p3 :: a -> a -> a -> Point V3 a
p3 x y z = P (V3 x y z)
