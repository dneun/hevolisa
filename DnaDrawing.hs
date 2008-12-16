module DnaDrawing where

import DnaPolygon (DnaPolygon)

data DnaDrawing = DnaDrawing [DnaPolygon] deriving (Show)