--
-- Module      : Renderer
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Renderer (drawingError) where

import Control.Monad
import Data.Word
import Data.Array.MArray
import qualified Data.Traversable as T
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.Pixmap as P
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.General.Structs hiding (Color)
import DnaDrawing
import DnaPolygon
import qualified DnaBrush as B
import DnaPoint
import qualified Settings as S


data Color a = Color {
      red   :: a,
      green :: a,
      blue  :: a,
      alpha :: a
}

class Renderable a where
    render :: a -> C.Render ()

instance Renderable DnaPolygon where
    render p = do
      render (brush p)
      sequence $ map (\(DnaPoint x y) -> C.lineTo x y) (points p)
      C.fill

instance Renderable B.DnaBrush where
    render br = C.setSourceRGBA r g b a
        where r = normalize B.red br
              g = normalize B.green br
              b = normalize B.blue br
              a = normalize B.alpha br
              normalize f = (/255) . fromIntegral . f

instance Renderable DnaDrawing where
    render = sequence_ . map render . polygons


-- | 1. Rasterize the drawing
--
-- 2. Load an image from a file and rasterize it
--
-- 3. Compare the color values of the drawing and the image pixel by pixel
drawingError :: DnaDrawing       -- ^ the drawing is rasterized
             -> FilePath         -- ^ rasterize an image from a file
             -> IO (Maybe Word8) -- ^ return the color pixel error
drawingError d path = do
  drawingPixbuf <- renderToPixbuf $ render d
  imagePixbuf <- fileToPixbuf path
  T.sequence $ liftM2 error drawingPixbuf imagePixbuf
      where
        error :: Pixbuf -> Pixbuf -> IO Word8
        error p1 p2 = do
          colors1 <- colors p1
          colors2 <- colors p2
          return $ sum $ zipWith colorError colors1 colors2

        colors :: Pixbuf -> IO [Color Word8]
        colors p = do pix <- pixbufGetPixels p :: IO (G.PixbufData Int Word8)
                      els <- getElems pix
                      return $ toColors els

        toColors :: (Num a) => [a] -> [Color a]
        toColors []         = []
        toColors (r:g:b:xs) = Color r g b 255 : toColors xs
        toColors _          = Prelude.error "wrong number of arguments"
       
        colorError :: (Num a) => Color a -> Color a -> a
        colorError x y = delta red   * delta red +
                         delta green * delta green +
                         delta blue  * delta blue
                             where delta f = f x - f y
   
        fileToPixbuf :: FilePath -> IO (Maybe Pixbuf)
        fileToPixbuf fp = C.withImageSurfaceFromPNG fp $ renderToPixbuf . renderSurface

        renderSurface :: C.Surface -> C.Render ()
        renderSurface s = C.setSourceSurface s 0 0 >> C.paint


-- | Rasterisation: FIXME
renderToPixbuf :: C.Render () -> IO (Maybe Pixbuf)
renderToPixbuf render = do
  G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  G.set window [G.containerChild G.:= canvas]
  G.widgetShowAll window
  G.flush
  pixbuf <- updateCanvas canvas render
  G.widgetDestroy window
  return pixbuf

  where updateCanvas :: G.DrawingArea -> C.Render () -> IO (Maybe Pixbuf)
        updateCanvas canvas act = do
                     win <- widgetGetDrawWindow canvas
                     G.renderWithDrawable win act
                     pixmap <- P.pixmapNew (Just win) width height Nothing
                     pixbuf <- pixbufGetFromDrawable pixmap (Rectangle 0 0 width height)
                     return pixbuf
        height = truncate S.maxHeight :: Int
        width  = truncate S.maxWidth  :: Int