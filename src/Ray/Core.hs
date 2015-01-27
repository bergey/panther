{-# LANGUAGE DeriveFunctor #-}

-- | The main rendering infrastructure, as parameterized by the Algo type.

module Ray.Core where

import Ray.Types
import Ray.Camera (getInverseCamMatrix)
import Ray.Image (asImg)
import Ray.Shapes

import Codec.Picture
import Linear
import Linear.Affine
import Data.Array

import Control.Applicative
import Data.Traversable
import Control.Lens

-- IO needed for future PRNG
render :: Algo -> Scene -> IO (Image PixelF)
render = runM getImg

renderArray :: Algo -> Scene -> IO (Array (V2 Int) Spectrum)
renderArray = runM getSpectralArray

getImg :: M (Image PixelF)
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

enumCoords :: V2 Int -> [V2 Int]
enumCoords (V2 xres yres) = [V2 x y | x <- [0..xres-1], y <- [0..yres-1]]

enumPixelCoords :: M [V2 Int]
enumPixelCoords = enumCoords <$> view (algorithms . resolution)

mkImgSample :: M44 Double -> V2 Double -> ImgSample Ray
mkImgSample m xy = ImgSample xy $ Ray origin (globalRay m xy) 1

globalRay :: M44 Double -> V2 Double -> V3 Double
globalRay m u = view _xyz $ m !* camRay u

camRay :: V2 Double -> V4 Double
camRay (V2 x y) = V4 x y 1 1

getCameraRays :: M (Array (V2 Int) [ImgSample Ray])
getCameraRays = do
    m <- getInverseCamMatrix
    ss <- getSampleLocs
    return $ (fmap . fmap) (mkImgSample m) ss

getSampleLocs :: M (Array (V2 Int) [V2 Double])
getSampleLocs = do
    sampler <- view $ algorithms . imageSampler
    n <- view $ algorithms . samplesPerPixel
    pxs <- enumPixelCoords
    ss <- traverse (sampler n) pxs
    res <- view $ algorithms . resolution
    return $ array (0, res) $ zip pxs ss

getIntersection :: Ray -> M (Maybe Intersection)
getIntersection r = intersect <$> view scene <*> pure r

radiance :: Ray -> M Spectrum
radiance ray = do
    x <- getIntersection ray
    n <- view $ algorithms . samplesPerCameraRay
    integrator <- view $ algorithms . surfaceIntegrator
    case x of
     Nothing -> view $ scene . background
     Just isect -> integrator n ray isect

-- r2f :: Spectrum -> PixelF -- Double -> Float
