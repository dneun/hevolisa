--
-- Module      : Renderer
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Renderer (drawingError,drawingToFile) where

import Control.Monad
import Data.ByteString (unpack)
import Data.Word
import qualified Data.Traversable as T
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.Pixmap as P
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.General.Structs hiding (Color)
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Shapes.DnaPolygon
import qualified Hevolisa.Shapes.DnaBrush as B
import Hevolisa.Shapes.DnaPoint
import qualified Hevolisa.Settings as S
--import Debug.Trace


data Color a = Color {
      red   :: a,
      green :: a,
      blue  :: a,
      alpha :: a
} deriving (Show)

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
        where r = normalize B.red br
              g = normalize B.green br
              b = normalize B.blue br
              a = normalize B.alpha br
              normalize f = (/255) . fromIntegral . f

instance Renderable DnaDrawing where
    render = render . polygons

instance (Renderable a) => Renderable [a] where
    render = mapM_ render


-- | 1. Rasterize the drawing
--
-- 2. Load an image from a file and rasterize it
--
-- 3. Compare the color values of the drawing and the image pixel by pixel
drawingError :: DnaDrawing  -- ^ the drawing is rasterized
             -> FilePath    -- ^ rasterize an image from a file
             -> IO Integer  -- ^ return the color pixel error
drawingError d path = do
  drawing <- toSurface $ render d
  C.withImageSurfaceFromPNG path $ \image -> error drawing image
      where
        error :: C.Surface -> C.Surface -> IO Integer
        error s1 s2 = do
          colors1 <- colors s1
          colors2 <- colors s2
          return $ sum $ zipWith colorError colors1 colors2

        colors :: C.Surface -> IO [Color Integer]
        colors s = C.imageSurfaceGetData s >>=
                   return . toColors . map fromIntegral . unpack

        toColors :: (Num a) => [a] -> [Color a]
        toColors []           = []
        toColors (r:g:b:a:xs) = Color r g b a : toColors xs
        toColors _            = Prelude.error "wrong number of arguments"
       
        colorError :: (Num a) => Color a -> Color a -> a
        colorError x y = deltaRed   * deltaRed +
                         deltaGreen * deltaGreen +
                         deltaBlue  * deltaBlue
            where deltaRed   = red x - red y
                  deltaGreen = green x - green y
                  deltaBlue  = blue x - blue y

        toSurface :: C.Render () -> IO C.Surface
        toSurface r = do surface <- C.createImageSurface C.FormatRGB24 width height
                         C.renderWith surface r
                         return surface

drawingToFile :: DnaDrawing -> IO ()
drawingToFile d = C.withImageSurface C.FormatRGB24 width height $ \result -> do
                    C.renderWith result $ render d
                    C.surfaceWriteToPNG result "result.png"

height = truncate S.maxHeight :: Int
width  = truncate S.maxWidth  :: Int