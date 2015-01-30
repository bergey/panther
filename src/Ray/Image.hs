-- | Converting between pixel-centric and ray-centric models.

module Ray.Image where

import Ray.Types

import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Jpg (encodeJpeg)
import Linear
import Data.Array
import Data.Word
import qualified Data.ByteString.Lazy as BS

clamp :: Float -> Float
clamp x | x > 1 = 1
clamp x | x < 0 = 0
clamp x = x

bit8 :: Float -> Word8
bit8 px = round (255 * clamp px)

bit16 :: Float -> Word16
bit16 px = round (65536 * px)

to8Bit :: Image PixelF -> Image Pixel8
to8Bit = pixelMap bit8

to16Bit :: Image PixelF -> Image Pixel16
to16Bit = pixelMap bit16

toJpg :: Image PixelF -> Image PixelYCbCr8
toJpg = convertImage . (promoteImage :: Image Pixel8 -> Image PixelRGB8) . to8Bit

-- saveJpeg :: FilePath -> Image PixelF -> IO ()
-- saveJpeg fp img = BS.writeFile fp . encodeJpeg . toJpg $ img

asImg :: V2 Int -> Array (V2 Int) Spectrum -> Image PixelRGBF
asImg (V2 xres yres) arr = generateImage lookup xres yres where
  lookup x y = asRGB $ arr ! V2 x y

asRGB :: V3D -> PixelRGBF
asRGB (V3 r g b) = PixelRGBF (r2f  r) (r2f g) (r2f b)

asRGB8 :: PixelRGBF -> PixelRGB8
asRGB8 (PixelRGBF r g b) = PixelRGB8 (bit8 r) (bit8 g) (bit8 b)

asRGB16 :: PixelRGBF -> PixelRGB16
asRGB16 (PixelRGBF r g b) = PixelRGB16 (bit16 r) (bit16 g) (bit16 b)
