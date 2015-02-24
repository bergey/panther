-- | Main rendering functions.

module Ray.Render where

import Ray.Types

-- renderIO :: Algo -> Scene -> IO (Image PixelF)
-- renderIO a s = withSystemRandom $ render a s

render :: Algo -> Scene -> Image PixelF
render a s = runReader renderM (MRead a s)

renderImage :: M (Image PixelF)
renderImage = do
    V2 xres yres <- view $ algorithms . resolution
    img <- renderArray
    let px x y = r2f $ img ! (V2 x y)
    return $ generateImage

renderArray :: M (Array2D Spectrum)
renderArray = do
