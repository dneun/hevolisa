--
-- Module      : Renderer
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Renderer where

import Control.Monad
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import DnaDrawing
import DnaPolygon
import DnaBrush
import DnaPoint
import qualified Settings as S


display :: DnaDrawing -> IO ()
display d = do
  G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  G.widgetSetSizeRequest window (truncate S.maxWidth) (truncate S.maxHeight)
  -- press any key to quit
  G.onKeyPress window $ const (do G.widgetDestroy window; return True)
  G.onDestroy window G.mainQuit
  surface <- render d
  G.onExpose canvas $ const $ do drawWin <- G.widgetGetDrawWindow canvas
                                 G.renderWithDrawable drawWin $ do
                                   C.setSourceSurface surface 0 0
                                   C.paint
                                   return True
  G.set window [G.containerChild G.:= canvas]
  G.widgetShowAll window
  G.mainGUI

render :: DnaDrawing -> IO C.Surface
render d = do
  let width  = (truncate S.maxWidth)
      height = (truncate S.maxHeight)
  surface <- C.createImageSurface C.FormatARGB32 width height
  C.renderWith surface $ renderPolygons d
  return surface

renderPolygons :: DnaDrawing -> C.Render [()]
renderPolygons d = sequence $ map renderPolygon (polygons d)

renderPolygon :: DnaPolygon -> C.Render ()
renderPolygon p = do renderBrush $ brush p
                     sequence $ map (\(DnaPoint x y) -> C.lineTo x y) (points p)
                     C.fill

renderBrush :: DnaBrush -> C.Render ()
renderBrush br = C.setSourceRGBA r g b a
    where r = normalize red br
          g = normalize green br
          b = normalize blue br
          a = normalize alpha br
          normalize f = (/255) . fromIntegral . f


error :: C.Surface -> C.Surface -> IO Double
error = undefined