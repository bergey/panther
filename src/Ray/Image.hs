-- | Converting between pixel-centric and ray-centric models.

module Ray.Image where

import Ray.Types

import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Jpg (encodeJpeg)
import Linear
import Data.Array
import qualified Data.ByteString.Lazy as BS

clamp :: Float -> Float
clamp x | x > 1 = 1
clamp x | x < 0 = 0
clamp x = x

to8Bit :: Image PixelF -> Image Pixel8
to8Bit = pixelMap $ \px -> round (255 * clamp px)

to16Bit :: Image PixelF -> Image Pixel16
to16Bit = pixelMap $ \px -> round (65536 * px)

toJpg :: Image PixelF -> Image PixelYCbCr8
toJpg = convertImage . (promoteImage :: Image Pixel8 -> Image PixelRGB8) . to8Bit

saveJpeg :: FilePath -> Image PixelF -> IO ()
saveJpeg fp img = BS.writeFile fp . encodeJpeg . toJpg $ img

asImg :: V2 Int -> Array (V2 Int) Spectrum -> Image PixelF
asImg (V2 xres yres) arr = generateImage lookup xres yres where
  lookup x y = r2f $ arr ! V2 x y
