-- | Rendering algorithms, including various sampling and integration strategies.

module Ray.Algorithms where

import Ray.Types
import Ray.Shapes

import Linear
import Linear.Affine

import Control.Applicative
import Control.Lens
import Data.Maybe

-- import qualified System.Random.MWC as R

naiveRenderer res = Algo {
    _samplesPerPixel = 1,
    _samplesPerCameraRay = 10,
    _resolution = res,
    _imageSampler = onGridSampler,
    _surfaceIntegrator = directLightIntegrator,
    _imageReconstructor = boxFilter
    }

halfPixel :: V2 Double
halfPixel = V2 (0.5) (0.5)

-- | onGridSampler ignores the number of pixels, and picks the center
-- of each square pixel sample region.
onGridSampler :: ImageSampler
onGridSampler _ px = return $ [halfPixel + (r2f <$> px)]

boxFilter :: ImageReconstructor
boxFilter = fmap $ mean . fmap _sampleValue
  where
    mean :: Fractional n => [n] -> n
    mean xs = sum xs / (r2f $ length xs)

-- uniform :: Variate a => M a
-- uniform = state R.uniform

-- uniformSampler1D :: Sampler1D
-- uniformSampler1D n = replicate n $ uniform

-- uniformSampler2D n = sequence . replicate n $ do

-- | @ambientintegrator@ ignores light sources, and returns the
-- color of the surface directly.
ambientIntegrator :: SurfaceIntegrator
ambientIntegrator _n _ (Intersection _ _ b) = return $ b

-- | @directLightIntegrator@ measures direct lighting, ignoring any
-- paths with multiple bounces.
directLightIntegrator :: SurfaceIntegrator
directLightIntegrator n (Ray p u _s) (Intersection ds _n m) = do
    ls <- view $ scene . lights
    os <- view $ scene . visibles
    let
        q = p .+^ u ^* sqrt ds
        rays = [Ray q (lightDirection q l) 1 | l <- ls]
        intersections = intersect os <$> rays
        li = catMaybes $ zipWith (unshadowed q) intersections ls
    return $ m * sum li

unshadowed :: P3D -> Maybe Intersection -> Light -> Maybe Spectrum
unshadowed _ (Just _) _ = Nothing
unshadowed p Nothing l = Just $ lightSpectrum p l

lightDirection :: P3D -> Light -> V3D
lightDirection u (PointLight v _) = v .-. u
lightDirection _ (ParallelLight v _) = v

-- | light spectrum reduced by distance.  This does not account for
-- intervening objects or participating media.
lightSpectrum :: P3D -> Light -> Spectrum
lightSpectrum u (PointLight v s) = s / qd v u
lightSpectrum _ (ParallelLight _ s) = s
