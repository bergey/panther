-- | Rendering algorithms, including various sampling and integration strategies.

module Ray.Algorithms where

import Ray.Types

import Linear
import Control.Applicative

-- import qualified System.Random.MWC as R

naiveRenderer res = Algo {
    _samplesPerPixel = 1,
    _samplesPerCameraRay = 10,
    _resolution = res,
    _imageSampler = onGridSampler,
    _surfaceIntegrator = ambientIntegrator,
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
