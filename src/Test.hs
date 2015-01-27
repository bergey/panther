module Main where

import           Ray.Core
import           Ray.Image
import           Ray.Materials
import           Ray.Shapes
import           Ray.Types
import           Ray.Algorithms
import           Ray.Camera
import           Solve

import           Linear
import           Linear.Affine
import           Linear.Projection
-- import           System.Random.MWC
import Codec.Picture
import Codec.Picture.Types
import Codec.Picture.Jpg (encodeJpeg)

import Data.Word
import           Control.Lens
import           Data.Distributive
import           Data.List
import           Data.Ord
import           Data.Maybe
import           Control.Monad.ST
import qualified Data.ByteString.Lazy as BS
import Control.Applicative

main :: IO ()
main = do
    img <- render (naiveRenderer $ V2 640 480) testScene
    writePng "raytracer-test.png" $ to8Bit img
    -- saveJpeg "raytracer-test.jpg" img
    -- BS.writeFile "jp-debug.png" . encodePng $ generateImage lookup 640 480 where
    --   lookup _ _ = (128 :: Word8)

run :: M a -> IO a
run m = runM m (naiveRenderer $ V2 640 480) testScene

m :: IO (M44 Double)
m = run getInverseCamMatrix

getRay :: V2 Double -> IO (ImgSample Ray)
getRay u = run $ do
    m <- getInverseCamMatrix
    o <- view $ scene . camera . eye
    res <- view (algorithms . resolution)
    let invRes = 1 / (r2f <$> res)
    return $ mkImgSample o invRes m u
