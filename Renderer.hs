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
import Foreign.Storable
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.Pixmap as P
import Graphics.UI.Gtk.Gdk.DrawWindow
import Graphics.UI.Gtk.Gdk.Pixbuf
import Graphics.UI.Gtk.General.Structs hiding (Color)
import Graphics.UI.Gtk.Gdk.Drawable
import DnaDrawing
import DnaPolygon
import qualified DnaBrush as B
import DnaPoint
import qualified Settings as S
import ColorMatrix


-- display :: DnaDrawing -> IO ()
-- display d = do
--   G.initGUI
--   window <- G.windowNew
--   canvas <- G.drawingAreaNew
--   G.widgetSetSizeRequest window (truncate S.maxWidth) (truncate S.maxHeight)
--   -- press any key to quit
--   G.onKeyPress window $ const (do G.widgetDestroy window; return True)
--   G.onDestroy window G.mainQuit
--   surface <- render d
--   G.onExpose canvas $ const $ do drawWin <- G.widgetGetDrawWindow canvas
--                                  G.renderWithDrawable drawWin $ do
--                                    C.setSourceSurface surface 0 0
--                                    C.paint
--                                    return True
--   G.set window [G.containerChild G.:= canvas]
--   G.widgetShowAll window
--   G.mainGUI

-- render :: DnaDrawing -> IO C.Surface
-- render d = do
--   let width  = (truncate S.maxWidth)
--       height = (truncate S.maxHeight)
--   surface <- C.createImageSurface C.FormatARGB32 width height
--   C.renderWith surface $ renderPolygons d
--   return surface

data Color a = Color {
      red   :: a,
      green :: a,
      blue  :: a,
      alpha :: a
}

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

renderPolygons :: DnaDrawing -> C.Render ()
renderPolygons = sequence_ . map renderPolygon . polygons
    where
      renderPolygon :: DnaPolygon -> C.Render ()
      renderPolygon p = do
        renderBrush $ brush p
        sequence $ map (\(DnaPoint x y) -> C.lineTo x y) (points p)
        C.fill

      renderBrush :: B.DnaBrush -> C.Render ()
      renderBrush br = C.setSourceRGBA r g b a
          where r = normalize B.red br
                g = normalize B.green br
                b = normalize B.blue br
                a = normalize B.alpha br
                normalize f = (/255) . fromIntegral . f



drawingError :: DnaDrawing -> String -> IO (Maybe Word8)
drawingError d path = do
  drawingPixbuf <- renderToPixbuf (renderPolygons d)
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