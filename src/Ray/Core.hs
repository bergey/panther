{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}

-- | The main rendering infrastructure, as parameterized by the Algo type.

module Ray.Core where

import Ray.Types
import Ray.Camera
import Ray.Image
import Ray.Shapes
import Ray.Imports

import Codec.Picture
import Data.Array

render :: Algo -> Scene -> IO (Image PixelRGBF)
render = runM getImg

renderArray :: Algo -> Scene -> IO (Array2D Spectrum)
renderArray = runM getSpectralArray

getImg :: M (Image PixelRGBF)
getImg = do
    res <- view $ algorithms . resolution
    tm <- view $ algorithms . toneMapping
    spectrum <- getSpectralArray
    return $ asImg res $ tm spectrum

getSpectralArray :: M (Array2D Spectrum)
getSpectralArray = do
    recon <- view $ algorithms . imageReconstructor
    recon <$> getSpectralSamples

getSpectralSamples :: M (Array2D [ImgSample Spectrum])
getSpectralSamples = overSamples radiance =<< getCameraRays

overSamples :: (a -> M b) ->
               Array2D [ImgSample a] -> M (Array2D [ImgSample b])
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

getCameraRays :: M (Array2D [ImgSample Ray])
getCameraRays = do
    m <- getInverseCamMatrix
    ss <- getSampleLocs
    o <- view $ scene . camera . eye
    res <- view (algorithms . resolution)
    let invRes = 1 / (r2f <$> res)
    return $ (fmap . fmap) (mkImgSample o invRes m) ss

getSampleLocs :: M (Array2D [V2 Double])
getSampleLocs = do
    sampler <- view $ algorithms . imageSampler
    n <- view $ algorithms . samplesPerPixel
    pxs <- enumPixelCoords
    ss <- traverse (sampler n) pxs
    res <- view $ algorithms . resolution
    return $ array (0, res - 1) $ zip pxs ss

getIntersection :: Ray -> M (Option (Intersection Spectrum))
getIntersection r = intersect <$> view scene <*> pure r

radiance :: Ray -> M Spectrum
radiance ray = do
    x <- getIntersection ray
    n <- view $ algorithms . samplesPerCameraRay
    integrator <- view $ algorithms . surfaceIntegrator
    case getOption x of
     Nothing -> view $ scene . background
     Just isect -> (ray ^. raySpectrum * ) <$> integrator n ray isect

p3 :: a -> a -> a -> Point V3 a
p3 x y z = P (V3 x y z)
