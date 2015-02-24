-- | Rendering algorithms, including various sampling and integration strategies.

module Ray.Algorithms where

import Ray.Types
import Ray.Shapes
import Ray.Util

import Prelude (Num(..), Floating(..), Fractional(..), Double, ($), (.), const,
                (!!), length, Int, Integer)
import Data.Foldable
import Linear
import Linear.Affine
import System.Random.MWC
import Numeric.Interval (Interval, (...))
import Data.Array

import Control.Applicative
import Control.Lens hiding ((...))
import Data.Maybe
import Data.List (genericLength)
import Control.Monad
import Control.Monad.Reader
import Data.Semigroup
import Data.Semigroup.Foldable

-- import qualified System.Random.MWC as R

naiveRenderer res = Algo {
    _samplesPerPixel = 1,
    _samplesPerCameraRay = 10,
    _resolution = res,
    _imageSampler = onGridSampler,
    _discreteSampler = uniformDiscreteSampler,
    _surfaceIntegrator = directLightIntegrator,
    _imageReconstructor = boxFilter,
    _toneMapping = uniformLinearTone
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
directLightIntegrator _n ray ix = do
    ls <- view $ scene . lamps
    os <- view $ scene . visibles
    return . sum $ directLightContribution os ray ix <$> ls

-- | @oneRandomlightintegrator@ picks a light at random for each sample, and
-- calculates the direct contribution from that light.
oneRandomLightIntegrator :: SurfaceIntegrator
oneRandomLightIntegrator n ray ix = do
    allLamps <- view $ scene . lamps
    lampSamples <- choose allLamps n
    os <- view $ scene . visibles
    let weight = genericLength allLamps / r2f n
    return . (* weight) . sum $ directLightContribution os ray ix <$> lampSamples

-- | @directLightContribution ls os ray ix@ calculates the light
-- reaching @ix@ directly from the light l, accounting for
-- shading by @os@.
directLightContribution :: [Object] -> Ray -> Intersection Spectrum -> Lamp ->
                          Spectrum
directLightContribution os ray ix l = reflectance * incoming
  where
    reflectance = ix ^. material
    incoming = directLightArriving os q (ix ^. tEpsilon) l
    q = intersectionPt ray ix

intersectionPt :: Ray -> Intersection a -> P3D
intersectionPt ray ix = (ray ^. rayOrigin) .+^ (ray ^. rayDir) ^* (ix ^. tHit)

-- | @directLightArriving os q ε l@ calculates the light reaching
-- point q from light l, accounting for possible shading from the
-- objects os and for the distance to the light.  @ε@ is the
-- uncertainty in the position @q@, used to avoid false
-- self-intersections.
directLightArriving :: [Object] -> P3D -> Double -> Lamp -> Spectrum
directLightArriving os q ε l = maybe s (const 0) (intersect os ray) where
  ray = Ray q (lampDirection q l) (ε...posInfinity) 1
  s = lampSpectrum q l

-- | The direction from the given point towards the given lamp.  For
-- Point & Parallel lamps, this is a single direction.
lampDirection :: P3D -> Lamp -> V3D
lampDirection u (PointLamp v _) = v .-. u
lampDirection _ (ParallelLamp v _) = -v

-- | Lamp spectrum reduced by distance.  This does not account for
-- intervening objects or participating media.
lampSpectrum :: P3D -> Lamp -> Spectrum
lampSpectrum u (PointLamp v s) = s ^/ qd v u
lampSpectrum _ (ParallelLamp _ s) = s

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

luminance :: Spectrum -> Double
luminance = dot (V3 0.212671 0.715160 0.072169)

data Mean a = Mean Integer a

getMean :: Fractional a => Mean a -> a
getMean (Mean ct a) = a / r2f ct

mkMean :: a -> Mean a
mkMean = Mean 1

instance Num a => Semigroup (Mean a) where
    Mean ctA a <> Mean ctB b = Mean (ctA+ctB) (a+b)

instance Ix i => Foldable1 (Array i)

-- | A uniform linear tone mapping due to Greg Ward.  As described in
-- Glassner 1995, p.1063.
uniformLinearTone :: ToneMapping
uniformLinearTone picture = fmap (* pure m) picture where
  pictureL :: Double
  pictureL = exp . getMean $ foldMap1 (mkMean . log . luminance) picture
  displayMaxL = 100
  m = ( (1.219 + (displayMaxL / 2)**0.4) / (1.219 + pictureL**0.4) )**2.5 / displayMaxL
