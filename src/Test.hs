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
    img <- render (naiveRenderer (V2 640 480)
                   & surfaceIntegrator .~ oneRandomLightIntegrator)
           testScene
    writePng "raytracer-test.png" $ to16Bit img

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

testScene = Scene {
    _camera = Camera (P (V3 0 0 5)) (P (V3 0 0 0)) (V3 0 1 0) (27 * pi / 180) 1,
    _background = 0, -- black
    _lights = [PointLight (P (V3 8 0 8)) 10],
    _visibles = [Object (Sphere (P (V3 0 0 0)) 1) 1]
    }
