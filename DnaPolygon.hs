module DnaPolygon (
                   DnaPolygon,

                   -- Accessors
                   polygonPointsCount,

                   -- Constructor, mutation
                   initPolygon,
                   mutatePolygon
                  ) where

import Settings
import Tools ( Mutable( mutate ), getRandomNumber, maybeMutate, removeElem )
import DnaBrush ( DnaBrush, initBrush )
import DnaPoint ( DnaPoint( DnaPoint ), initPoint, randomPoint, pointX, pointY )

-- |A polygon has a brush for color and a list of points
data DnaPolygon = DnaPolygon DnaBrush [DnaPoint] deriving (Show,Eq,Read)

-- |Get the brush of the polygon
polygonBrush :: DnaPolygon -> DnaBrush
polygonBrush (DnaPolygon brush _) = brush

-- |Get the points of the polygon
polygonPoints :: DnaPolygon -> [DnaPoint]
polygonPoints (DnaPolygon _ points) = points

-- |Count the points of the polygon
polygonPointsCount :: Integral a => DnaPolygon -> a
polygonPointsCount = fromIntegral . length . polygonPoints

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
mutatePolygon p = maybeAddPoint p >>= 
                  maybeRemovePoint >>= 
                  mutateBrush >>= 
                  mutatePoints

-- |Add a point if it`s time to do so
maybeAddPoint :: DnaPolygon -> IO DnaPolygon
maybeAddPoint p = maybeMutate activeAddPointMutationRate
                  (if (polygonPointsCount p < activePointsPerPolygonMax) then
                       addPointAtRandomIndex p else return p)
                  p

-- |Add a point at a random position between two points
addPointAtRandomIndex :: DnaPolygon -> IO DnaPolygon
addPointAtRandomIndex p@(DnaPolygon b pts) = do index <- getRandomNumber 1 (polygonPointsCount p - 1)
                                                return (DnaPolygon b (addPoint index pts))

-- |Add a point at the given position
addPoint :: Int -> [DnaPoint] -> [DnaPoint]
addPoint index pts = left ++ [DnaPoint newX newY] ++ right
    where left = take index pts
          right = drop index pts
          newX = (pointX prev + pointX next) / 2
          newY = (pointY prev + pointY next) / 2
          prev = last left
          next = head right

-- |Remove a point from the polygon
removePoint :: Int -> [DnaPoint] -> [DnaPoint]
removePoint = removeElem

-- |Remove a point if it`s time to do so
maybeRemovePoint :: DnaPolygon -> IO DnaPolygon
maybeRemovePoint p = maybeMutate activeRemovePointMutationRate
                     (if (polygonPointsCount p > activePointsPerPolygonMin) then
                          removePointAtRandomIndex p else return p)
                     p

-- |Remove a random point
removePointAtRandomIndex :: DnaPolygon -> IO DnaPolygon
removePointAtRandomIndex p@(DnaPolygon b pts) = do index <- getRandomNumber 0 (polygonPointsCount p)
                                                   return (DnaPolygon b (removePoint index pts))

-- |Mutate the polygon brush
mutateBrush :: DnaPolygon -> IO DnaPolygon
mutateBrush (DnaPolygon brush pts) = mutate brush >>= \b -> return (DnaPolygon b pts)

-- |Mutate the polygon points
mutatePoints :: DnaPolygon -> IO DnaPolygon
mutatePoints (DnaPolygon b pts) = do points <- sequence $ map mutate pts
                                     return (DnaPolygon b points)