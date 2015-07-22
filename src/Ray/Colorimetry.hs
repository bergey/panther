-- | Code & data to convert between spectra, colors, sRGB.

module Ray.Colorimetry where

import Data.Colour
import Ray.Types

import Prelude (Double, Int, realToFrac, IO, ($), Real, Fractional, Show, Num)
import qualified Data.Array as A
import Data.Foldable

class SpectrumClass s where
    toXYZ :: Num a => s a -> XYZ a

instance SpectrumClass RGBSpectrum where
    toXYZ rgbs= m !* rgbs where
        m = xyz
            (rgbSpectrum 1.055000 5.151300e-001 3.333600e-001)
            (rgbSpectrum 6.310000e-001 1.000100 3.790000e-002)
            (rgbSpectrum 9.056400e-004 5.857300e-003 1.616600)

instance A.Ix i => Foldable (A.Array i) where
    foldMap f = foldMap f . A.elems

noMapping :: ToneMapping
noMapping = fmap toXYZ

luminance :: Spectrum -> Double
luminance = view _y . toXYZ

meanLuminance :: Array2D Spectrum -> Double
meanLuminance arr = getSum (foldMap (Sum . luminance) arr) / arraySize arr

logMeanLuminance :: Array (V2 int) Spectrum -> Double
logMeanLuminance = sum / size where
  sum = getSum $ foldMap (Sum . logBase 10 . luminance)
  size = r2f $ arraySize arr

arraySize :: Array2D a -> Int
arraySize arr = x * y where
  (lo, hi) = bounds arr
  V2 x y = abs $ hi - lo

minLuminance :: Array2D Spectrum -> Double
minLuminance = foldl min 0 . fmap luminance

-- | The maximum luminance in a scene, or 1e9, whichever is smaller.
maxLuminance = foldl max 1e9
