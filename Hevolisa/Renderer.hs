--
-- Module      : Renderer
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Renderer (Color,drawingError,imageColors,drawingToFile) where

import Data.ByteString (unpack)
import qualified Graphics.Rendering.Cairo as C
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Shapes.DnaPolygon
import qualified Hevolisa.Shapes.DnaBrush as B
import Hevolisa.Shapes.DnaPoint
import qualified Hevolisa.Settings as S


data Color a = Color {
      red   :: a,
      green :: a,
      blue  :: a,
      alpha :: a
} deriving (Show,Eq)

class Renderable a where
    render :: a -> C.Render ()

instance Renderable DnaPoint where
    render (DnaPoint x y) = C.lineTo x y

instance Renderable DnaPolygon where
    render p = do
      render $ brush p
      render $ points p
      C.fill

instance Renderable B.DnaBrush where
    render br = C.setSourceRGBA r g b a
        where r = normalize $ B.red br
              g = normalize $ B.green br
              b = normalize $ B.blue br
              a = normalize $ B.alpha br
              normalize = (/255) . fromIntegral

instance Renderable DnaDrawing where
    render = render . polygons

instance (Renderable a) => Renderable [a] where
    render = mapM_ render


-- | 1. Rasterize the drawing
--
-- 2. Load an image from a file and rasterize it
--
-- 3. Compare the color values of the drawing and the image pixel by pixel
drawingError :: DnaDrawing -- ^ the drawing is rasterized
             -> [Integer]  -- ^ rasterize an image from a file
             -> IO Integer -- ^ return the color pixel error
drawingError d image = toSurface (render d) >>= removeAlpha >>= return . error image
    where
      error :: [Integer] -> [Integer] -> Integer
      error c1 c2 = sum $ zipWith (\x y -> (x - y)^2) c1 c2
                          
      toSurface :: C.Render () -> IO C.Surface
      toSurface r = do surface <- C.createImageSurface C.FormatRGB24 width height
                       C.renderWith surface r
                       return surface

-- | remove the alpha channel
removeAlpha :: C.Surface -> IO [Integer]
removeAlpha s = C.imageSurfaceGetData s >>=
                return . remove . map fromIntegral . unpack
                    where
                      remove :: [a] -> [a]
                      remove []           = []
                      remove (r:g:b:a:xs) = r:g:b:remove xs
                      remove _            = Prelude.error "wrong number of color values"

imageColors :: FilePath -> IO [Integer]
imageColors fp = C.withImageSurfaceFromPNG fp removeAlpha
                 
drawingToFile :: DnaDrawing -> IO ()
drawingToFile d = C.withImageSurface C.FormatRGB24 width height $ \result -> do
                    C.renderWith result $ render d
                    C.surfaceWriteToPNG result "result.png"

height = truncate S.maxHeight :: Int
width  = truncate S.maxWidth  :: Int