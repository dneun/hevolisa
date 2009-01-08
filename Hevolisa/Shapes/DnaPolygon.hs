--
-- Module      : DnaPolygon
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Shapes.DnaPolygon (
                   DnaPolygon,

                   -- * Accessors
                   brush,
                   points
                  ) where

import Hevolisa.Settings
import Hevolisa.Tools
import Hevolisa.Shapes.DnaBrush ( DnaBrush )
import Hevolisa.Shapes.DnaPoint ( DnaPoint( DnaPoint ), randomPoint, pointX, pointY )

-- |A polygon has a brush for color and a list of points
data DnaPolygon = DnaPolygon {
      brush  :: DnaBrush, 
      points :: [DnaPoint] 
} deriving (Show,Eq,Read)

-- |Count the points of the polygon
instance Points DnaPolygon where
    pointCount = fromIntegral . length . points

-- |Initialize the polygon with random garbage
instance RandomInit DnaPolygon where
    randomInit = do points <- randomPoints activePointsPerPolygonMin
                    brush <- randomInit
                    return (DnaPolygon brush points)
        where randomPoints :: Integer -> IO [DnaPoint]
              randomPoints n = randomInit >>=
                               sequence . replicate (fromIntegral n) . randomPoint

-- |A polygon has mutable DNA
instance Mutable DnaPolygon where
    mutate = mutatePolygon

-- |Mutate a polygon by adding and removing points and other funny tricks
mutatePolygon :: DnaPolygon -> IO DnaPolygon
mutatePolygon p = maybeAddPoint p >>= 
                  maybeRemovePoint >>= 
                  mutateBrush >>= 
                  mutatePoints
    where
      -- |Add a point if it`s time to do so
      maybeAddPoint :: DnaPolygon -> IO DnaPolygon
      maybeAddPoint p = maybeMutate activeAddPointMutationRate
                        (if (pointCount p < activePointsPerPolygonMax) then
                             addPointAtRandomIndex p else return p)
                        p

      -- |Add a point at a random position between two points
      addPointAtRandomIndex :: DnaPolygon -> IO DnaPolygon
      addPointAtRandomIndex p = do index <- getRandomNumber 1 (pointCount p - 1)
                                   return (p { points = addPoint index (points p) })

      -- |Add a point at the given position
      addPoint :: Int -> [DnaPoint] -> [DnaPoint]
      addPoint index pts = left ++ [DnaPoint newX newY] ++ right
          where left = take index pts
                right = drop index pts
                newX = (pointX prev + pointX next) / 2
                newY = (pointY prev + pointY next) / 2
                prev = last left
                next = head right

      -- |Remove a point if it`s time to do so
      maybeRemovePoint :: DnaPolygon -> IO DnaPolygon
      maybeRemovePoint p = maybeMutate activeRemovePointMutationRate
                           (if (pointCount p > activePointsPerPolygonMin) then
                                removePointAtRandomIndex p else return p)
                           p

      -- |Remove a random point
      removePointAtRandomIndex :: DnaPolygon -> IO DnaPolygon
      removePointAtRandomIndex p@(DnaPolygon b pts) = do 
        index <- getRandomNumber 0 (pointCount p)
        return (DnaPolygon b (removePoint index pts))

      -- |Remove a point from the polygon
      removePoint :: Int -> [DnaPoint] -> [DnaPoint]
      removePoint = removeElem

      -- |Mutate the polygon brush
      mutateBrush :: DnaPolygon -> IO DnaPolygon
      mutateBrush (DnaPolygon brush pts) = mutate brush >>= \b -> return (DnaPolygon b pts)

      -- |Mutate the polygon points
      mutatePoints :: DnaPolygon -> IO DnaPolygon
      mutatePoints p = (mapM mutate . points) p >>= 
                       return . DnaPolygon (brush p)