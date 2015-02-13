-- | Rendering algorithms, including various sampling and integration strategies.

module Ray.Algorithms where

import Ray.Types
import Ray.Shapes
import Ray.Util

import Linear
import Linear.Affine
import System.Random.MWC
import Numeric.Interval (Interval, (...))

import Control.Applicative
import Control.Lens hiding ((...))
import Data.Maybe
import Data.List (genericLength)
import Control.Monad
import Control.Monad.Reader

-- import qualified System.Random.MWC as R

naiveRenderer res = Algo {
    _samplesPerPixel = 1,
    _samplesPerCameraRay = 10,
    _resolution = res,
    _imageSampler = onGridSampler,
    _discreteSampler = uniformDiscreteSampler,
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
ambientIntegrator _n _ ix = return $ ix ^. material

-- | @directLightIntegrator@ measures direct lighting, ignoring any
-- paths with multiple bounces.  It ignores the number of samples
-- parameter.
directLightIntegrator :: SurfaceIntegrator
directLightIntegrator n ray ix = do
    let q = intersectionPt ray ix
    ls <- view $ scene . lights
    li <- traverse (directLightContribution q (ix ^. tEpsilon)) ls
    return $ ix ^. material * sum li

intersectionPt :: Ray -> Intersection a -> P3D
intersectionPt ray ix = (ray ^. rayOrigin) .+^ (ray ^. rayDir) ^* (ix ^. tHit)

directLightContribution :: P3D -> Double -> Light -> M Spectrum
directLightContribution q ε l = do
    os <- view $ scene . visibles
    let ray = Ray q (lightDirection q l) (ε...posInfinity) 1
    return $ unshadowed q (intersect os ray) l

unshadowed :: P3D -> Maybe (Intersection Spectrum) -> Light -> Spectrum
unshadowed _ (Just _) _ = 0
unshadowed p Nothing l = lightSpectrum p l

lightDirection :: P3D -> Light -> V3D
lightDirection u (PointLight v _) = v .-. u
lightDirection _ (ParallelLight v _) = -v

-- | light spectrum reduced by distance.  This does not account for
-- intervening objects or participating media.
lightSpectrum :: P3D -> Light -> Spectrum
lightSpectrum u (PointLight v s) = s ^/ qd v u
lightSpectrum _ (ParallelLight _ s) = s

-- | @oneRandomlightintegrator@ picks a light at random for each sample, and
-- calculates the direct contribution from that light.
oneRandomLightIntegrator :: SurfaceIntegrator
oneRandomLightIntegrator n ray ix = do
    let q = intersectionPt ray ix
    allLights <- view $ scene . lights
    lightSamples <- choose allLights n
    li <- traverse (directLightContribution q (ix ^. tEpsilon)) lightSamples
    return $ (ix ^. material) * genericLength allLights * sum li ^/ r2f n

-- TODO use distributions from statistics pkg here?
-- | @choose as n@ picks n random samples from as (uniformly distributed)
choose :: [a] -> Int -> M [a]
choose as n = do
    sampler <- view $ algorithms . discreteSampler
    is <- sampler l n
    return $ fmap (as !!) is
    where
      l = length as

uniformDiscreteSampler :: DiscreteSampler
uniformDiscreteSampler r n = replicateM n $ do
    g <- view gen
    liftIO $ uniformR (0,r-1) g
