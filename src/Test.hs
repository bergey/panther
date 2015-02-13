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
-- import           Data.List ()
import           Data.Ord
import           Data.Maybe
import           Control.Monad.ST
import qualified Data.ByteString.Lazy as BS
import Control.Applicative

main :: IO ()
main = do
    img <- render (naiveRenderer (V2 640 480))
                  -- & surfaceIntegrator .~ oneRandomLightIntegrator )
           -- & surfaceIntegrator .~ ambientIntegrator )
           testScene
    writePng "raytracer-test.png" $ pixelMap asRGB16 img

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

cam :: Double -> Camera
cam θ = Camera (p3 (5 * cos θ) (5 * sin θ) 0) 0 (V3 0 0 1) (27 * pi / 180) 1

testScene = Scene {
    _camera = cam (pi/4),
    _background = 0, -- black
    _lights = [PointLight (P (V3 8 0 8)) 40
              , PointLight (p3 0 0 (-8)) (V3 40 0 0) ],
    -- _lights = [ParallelLight (V3 0 0 (-1)) 100],
    _visibles = [Object (SSphere (Sphere 0 1)) 1]
    }
