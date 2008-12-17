module Renderer (
                 renderDrawing
                ,renderPolygon) where

import Shapes

renderDrawing :: Drawing (DrawPolygon Point) -> IO ()
renderDrawing d = sequence (map renderPolygon (polygons d)) >> return ()

renderPolygon :: DrawPolygon Point -> IO ()
renderPolygon = undefined