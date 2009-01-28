--
-- Module      : DnaDrawing
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Shapes.DnaDrawing ( DnaDrawing,
                                    -- * Accessors
                                    polygons
) where

import Hevolisa.Shapes.DnaPolygon ( DnaPolygon )
import Hevolisa.Tools
import Hevolisa.Settings

-- |A drawing contains an ordered set of polygons
data DnaDrawing = DnaDrawing {
      polygons :: [DnaPolygon] 
} deriving (Show,Eq,Read)


-- |Count the points in the drawing
instance Points DnaDrawing where
    pointCount = sum . map pointCount . polygons

-- |Construct and init a new Drawing
instance RandomInit DnaDrawing where
    randomInit = mseq (replicate min addPolygon) >>= return . DnaDrawing
        where min = fromIntegral activePolygonsMin
              mseq = foldl (>>=) (return [])

-- |Add a new polygon at a random position
addPolygon :: [DnaPolygon] -> IO [DnaPolygon]
addPolygon ps = do random <- getRandomNumber 0 (length ps)
                   polygon <- randomInit
                   return (addElem polygon random ps)

-- |Drawing has mutable DNA
instance Mutable DnaDrawing where
    mutate old = mutateDrawing old >>= change
        where change new | old == new = mutate old
                         | otherwise  = return new

-- |Basic drawing mutation function
mutateDrawing :: DnaDrawing -> IO DnaDrawing
mutateDrawing d = maybeAddPolygon d >>= 
                  maybeRemovePolygon >>= 
                  maybeMovePolygon >>= 
                  mutatePolygons
    where
      -- |Add a polygon if it`s time to do so and the constraints are met
      maybeAddPolygon :: DnaDrawing -> IO DnaDrawing
      maybeAddPolygon d = willMutate  activeAddPolygonMutationRate >>=
                          when (polygonsCount d < activePolygonsMax)
                               (applyToPolygons addPolygon) d
                    
      -- |Remove a polygon if it`s time to do so and the constraints are met
      maybeRemovePolygon :: DnaDrawing -> IO DnaDrawing
      maybeRemovePolygon d = willMutate activeRemovePolygonMutationRate >>=
                             when (polygonsCount d > activePolygonsMin)
                                  (applyToPolygons removePolygon) d

      -- |Remove a polygon at a random index
      removePolygon :: [DnaPolygon] -> IO [DnaPolygon]
      removePolygon p = do index <- getRandomNumber 0 (length p - 1)
                           return (removeElem index p)

      -- |Move a polygon if it`s time to do so and the constraints are met
      maybeMovePolygon :: DnaDrawing -> IO DnaDrawing
      maybeMovePolygon d = willMutate activeMovePolygonMutationRate >>=
                           when (polygonsCount d > 0)
                                (applyToPolygons movePolygon) d

      -- |Move a polygon in the list of polygons
      movePolygon :: [DnaPolygon] -> IO [DnaPolygon]
      movePolygon p = do from <- getRandomNumber 0 (length p - 1)
                         to   <- getRandomNumber 0 (length p - 1)
                         return (moveElem from to p)

      -- |Mutate polygons if it`s time to do so and the constraints are met
      mutatePolygons :: DnaDrawing -> IO DnaDrawing
      mutatePolygons = applyToPolygons (mapM mutate)

      -- |Apply a polygon function to a drawing
      applyToPolygons :: ([DnaPolygon] -> IO [DnaPolygon])
                      -> DnaDrawing
                      -> IO DnaDrawing
      applyToPolygons f d = f (polygons d) >>= return . DnaDrawing

      -- |Get the number of polygons of a drawing to check constraints
      polygonsCount :: Integral a => DnaDrawing -> a
      polygonsCount = fromIntegral . length . polygons