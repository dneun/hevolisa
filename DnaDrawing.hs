module DnaDrawing where

import DnaPolygon ( DnaPolygon, polygonPointsCount, initPolygon )
import Tools ( getRandomNumber )

data DnaDrawing = DnaDrawing [DnaPolygon] deriving (Show)

drawingPolygons :: DnaDrawing -> [DnaPolygon]
drawingPolygons (DnaDrawing pls) = pls

drawingPolygonsCount :: Integral a => DnaDrawing -> a
drawingPolygonsCount = fromIntegral . length . drawingPolygons

drawingPointCount :: DnaDrawing -> Integer
drawingPointCount = sum . map polygonPointsCount . drawingPolygons

initDrawing :: IO DnaDrawing
--initDrawing = do polygons <-
initDrawing = undefined

insertPolygon :: Int -> DnaPolygon -> [DnaPolygon] -> [DnaPolygon]
insertPolygon index new lst = left ++ [new] ++ right
    where left = take index lst
          right = drop index lst

addPolygon :: [DnaPolygon] -> IO [DnaPolygon]
addPolygon ps = do random <- getRandomNumber 0 (length ps)
                   polygon <- initPolygon
                   return (insertPolygon random polygon ps)