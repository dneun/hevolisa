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
  G.onExpose canvas $ const $ render d canvas
  G.set window [G.containerChild G.:= canvas]
  G.widgetShowAll window
  G.mainGUI

render :: DnaDrawing -> G.DrawingArea -> IO Bool
render d canvas = do
  win <- G.widgetGetDrawWindow canvas
  (width, height) <- G.widgetGetSize canvas
  G.renderWithDrawable win $ renderPolygons d
  return True

renderPolygons :: DnaDrawing -> C.Render [()]
renderPolygons d = sequence $ map renderPolygon (polygons d)

renderPolygon :: DnaPolygon -> C.Render ()
renderPolygon p = do renderBrush p
                     foldM_ renderLine (last $ points p) (points p)
                     C.fill

renderBrush p = C.setSourceRGBA r g b a
    where r = normalize red p
          g = normalize green p
          b = normalize blue p
          a = normalize alpha p
          normalize c = (/255) . fromIntegral . c . brush


renderLine :: DnaPoint -> DnaPoint -> C.Render DnaPoint
renderLine (DnaPoint x1 y1) (DnaPoint x2 y2) = do C.lineTo x2 y2
                                                  return (DnaPoint x2 y2)