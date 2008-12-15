module DnaPolygon where

import Settings
import DnaBrush
import DnaPoint

data DnaPolygon = DnaPolygon DnaBrush [DnaPoint]

polygonBrush :: DnaPolygon -> DnaBrush
polygonBrush (DnaPolygon brush _) = brush

polygonPoints :: DnaPolygon -> [DnaPoint]
polygonPoints (DnaPolygon _ points) = points

initPolygon :: IO DnaPolygon
initPolygon = do points <- randomPoints activePointsPerPolygonMin
                 brush <- initBrush
                 return (DnaPolygon brush points)

randomPoints :: Integer -> IO [DnaPoint]
randomPoints n = do origin <- initPoint
                    points <- sequence $ replicate (fromIntegral n) (randomPoint origin)
                    return points

randomPoint :: DnaPoint -> IO DnaPoint
randomPoint (DnaPoint x y) = do x <- mutateDim 3 maxWidth x
                                y <- mutateDim 3 maxHeight y
                                return (DnaPoint x y)

