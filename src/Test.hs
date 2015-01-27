module Main where

import           Ray.Core
import           Ray.Image
import           Ray.Materials
import           Ray.Shapes
import           Ray.Types
import           Ray.Algorithms
import           Solve

import           Linear
import           Linear.Affine
import           Linear.Projection
-- import           System.Random.MWC

import           Control.Lens
import           Data.Distributive
import           Data.List
import           Data.Ord
import           Data.Maybe
import           Control.Monad.ST
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
    -- prng <- create
    -- let img = toJpg $ render prng testScene
    -- BS.writeFile "raytracer-test.jpg" $ encodeJpeg img
    img <- render (naiveRenderer $ V2 640 480) testScene
    saveJpeg "raytracer-test.jpg" img

testScene :: Scene
testScene = Scene {
    _camera = Camera (P (V3 0 0 5)) (P (V3 0 0 0)) (V3 0 1 0) (pi * 27 / 180) 1,
    _background = 0, -- black
    _lights = [PointLight (P (V3 8 0 8)) 10],
    _visibles = [Object (Sphere (P (V3 0 0 0)) 1) 1]
    }

-- Everything below here should move to library modules
----------------------------------------
-- | a Ray through the center of the pixel
-- pixels origin is in the upper left, and coordinates increase down & right
-- rayThroughPixel :: Camera -> a -> Int -> Int -> Ray a
-- rayThroughPixel cam a x y = Ray (eye cam) (m !* v) a where
--   m = inverseInfinitePerspective (fov cam) aspectRatio (nearPlane cam) !*!
--       lookAt (eye cam) (lookAtPt cam .-. eye cam) (upDir cam)
--   (w, h) = resolution cam & both %~ fromIntegral
--   aspectRatio = w / h
--   v = V3 x' y' (nearPlane cam)
--   x' = 2 * (0.5 + fromIntegral x) / w - 1
--   y' = 2 * (0.5 + fromIntegral y) / w - 1

-- render :: Algo -> Scene -> Image PixelF
-- render algo scene = let (width, height) = resolution . camera $ scene in
--     runST . withImage width height $ \x y -> do
--         -- sample the pixel (x,y)
--         let ray = rayThroughPixel (camera scene) 1 x y
--             firstIntersection = minimumBy (comparing distance) .
--                                 catMaybes . distribute (visibles scene) $ ray
--         shade scene ray firstIntersection

-- Types of functions that I want
-- Some of these are provided upstream, and there's no reason to rename;
-- in those cases, these are just placeholders until I look up the names.

-- ImageReconstructor
-- :: Array (V2 Int) [ImgSample Spectrum] -> Array (V2 Int) Spectrum

-- If each sample only depends on samples within the same pixel, not
-- adjacent ones.  This function needs to localize the ImgSamples, by
-- subtracting the output pixel coordinates The Array (V2 Int)
-- [ImgSample Spectrum] stores absolute coordinates, to simplify code
-- (and avoid double differences) in filters with larger support.
-- fmap :: ([ImgSample Spectrum] -> Spectrum) -> ImageReconstructor
   -- Array (V2 Int) [ImgSample Spectrum] -> Array (V2 Int) Spectrum

-- fmap :: (a -> b) -> Array c a -> Array c b

-- traverse :: (a -> m b) -> Array c a -> M (Array c b)

-- shade :: Scene -> Ray Double -> Collision -> Float
-- shade scene (Ray _from _dist intensity) (Collision _ _ material) =
--     r2f $ intensity * material
