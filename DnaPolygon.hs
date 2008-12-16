module DnaPolygon (
                   DnaPolygon,

                   -- Accessors
                   polygonBrush,
                   polygonPoints,

                   -- Constructor, mutation
                   initPolygon,
                   mutatePolygon
                  ) where

import Settings
import Tools ( Mutable( mutate ), getRandomNumber )
import DnaBrush ( DnaBrush, initBrush)
import DnaPoint ( DnaPoint( DnaPoint ), initPoint, randomPoint, pointX, pointY )

-- |A polygon has a brush for color and a list of points
data DnaPolygon = DnaPolygon DnaBrush [DnaPoint] deriving (Show)

-- |Get the brush of the polygon
polygonBrush :: DnaPolygon -> DnaBrush
polygonBrush (DnaPolygon brush _) = brush

-- |Get the points of the polygon
polygonPoints :: DnaPolygon -> [DnaPoint]
polygonPoints (DnaPolygon _ points) = points

-- |Initialize the polygon with random garbage
initPolygon :: IO DnaPolygon
initPolygon = do points <- randomPoints activePointsPerPolygonMin
                 brush <- initBrush
                 return (DnaPolygon brush points)

-- |Create a list of random points
randomPoints :: Integer       -- ^ Number of points
             -> IO [DnaPoint] -- ^ return the result
randomPoints n = do origin <- initPoint
                    points <- sequence $ replicate (fromIntegral n) (randomPoint origin)
                    return points

-- |A polygon has mutable DNA
instance Mutable DnaPolygon where
    mutate = mutatePolygon

-- |Mutate a polygon by adding and removing points and other funny tricks
mutatePolygon :: DnaPolygon -> IO DnaPolygon
mutatePolygon p = maybeAddPoint p >>= maybeRemovePoint >>= mutateBrushInP >>= mutatePoints

-- |Coming soon...
maybeAddPoint p = undefined --do if (sum $ polygonPoints p < activePointsPerPolygonMax) then


addPoint :: Int -> [DnaPoint] -> [DnaPoint]
addPoint index pts = left ++ [DnaPoint newX newY] ++ right
    where left = take index pts
          right = drop index pts
          newX = (pointX prev + pointX next) / 2
          newY = (pointY prev + pointY next) / 2
          prev = last left
          next = head right

removePoint :: Int -> [DnaPoint] -> [DnaPoint]
removePoint index pts = left ++ right
    where left  = take index pts
          right = drop (index + 1) pts

maybeRemovePoint = undefined
mutateBrushInP = undefined
mutatePoints = undefined