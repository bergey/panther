-- |

module Ray.Camera where

import Ray.Types

import Linear
import Linear.Affine

import Control.Lens
import Control.Applicative
-- import Control.Monad.Reader

inverseCameraMatrix :: V2 Int -> Camera -> M44 Double
inverseCameraMatrix (V2 xres yres) cam = invLook !*! persp where
  invLook = case inv44 look of
      Just m -> m
      Nothing -> error "Impossible: affine transformation without inverse in inverseCameraMatrix"
  look = lookAt (cam ^. eye . _Point) (cam ^. lookAtPt . _Point) (cam ^. upDir)
  persp = inverseInfinitePerspective (cam ^. fov) aspect (cam ^. nearPlane)
  aspect = r2f xres / r2f yres

getInverseCamMatrix :: M (M44 Double)
getInverseCamMatrix =
    inverseCameraMatrix <$> view (algorithms . resolution) <*> view (scene . camera)

-- cameraRays :: Spectrum -> V2 Int -> M [ImageSample Ray]
-- cameraRays s = do
--     -- imgSampler <- reader $ _algorithms . _imageSampler
--     m <- inverseCameraMatrix
--     let ray (V2 x y) = Ray origin (m !* (V3 x y 1)) s
--     imgSampler <- view $ algorithms . imageSampler
--     n <- view $ algorithms . samplesPerPixel
--     imgCoords <- imgSampler n =<< pixelCoords

-- :: Spectrum -> SurfaceIntegrator -> Ray -> Maybe Intersection -> M Spectrum

sampleCamera = Camera (P (V3 0 0 5)) (P (V3 0 0 0)) (V3 0 1 0) (27 * pi / 180) 1
