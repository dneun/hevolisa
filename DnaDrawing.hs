-- |Intended usage: @ initDrawing >>= mutate >>= mutate >>= mutate @
module DnaDrawing (
                   DnaDrawing,

                   -- Constructors
                   initDrawing,
                   mapseq
) where

import DnaPolygon ( DnaPolygon, polygonPointsCount, initPolygon )
import Tools
import Settings

-- |A drawing contains an ordered set of polygons
data DnaDrawing = DnaDrawing [DnaPolygon] deriving (Show,Eq,Read)

-- |Get the polygons of a drawing
drawingPolygons :: DnaDrawing -> [DnaPolygon]
drawingPolygons (DnaDrawing pls) = pls

-- |Get the number of polygons of a drawing to check constraints
drawingPolygonsCount :: Integral a => DnaDrawing -> a
drawingPolygonsCount = fromIntegral . length . drawingPolygons

-- |Get the sum of the points of the polygons to check constraints
drawingPointCount :: DnaDrawing -> Integer
drawingPointCount = sum . map polygonPointsCount . drawingPolygons

-- |Sequences functions that produce actions
mapseq :: Monad m => m a -> [a -> m a] -> m a
mapseq f []     = f
mapseq s (f:fs) = mapseq (s >>= f) fs

-- |Construct and init a new Drawing
initDrawing :: IO DnaDrawing
initDrawing = do polygons <- mseq $ replicate min addPolygon
                 return (DnaDrawing polygons)
                     where min = fromIntegral activePolygonsMin
                           mseq = mapseq (return [])

-- |Add a new polygon at a random position
addPolygon :: [DnaPolygon] -> IO [DnaPolygon]
addPolygon ps = do random <- getRandomNumber 0 (length ps)
                   polygon <- initPolygon
                   return (addElem polygon random ps)

-- |Drawing has mutable DNA
instance Mutable DnaDrawing where
    mutate = mutateDrawing

-- |Apply a polygon function to a drawing
applyToPolygons :: ([DnaPolygon] -> IO [DnaPolygon])
                -> DnaDrawing
                -> IO DnaDrawing
applyToPolygons action (DnaDrawing pls) = do newpls <- action pls
                                             return (DnaDrawing newpls)

-- |Basic drawing mutation function
mutateDrawing :: DnaDrawing -> IO DnaDrawing
mutateDrawing d = maybeAddPolygon d >>= 
                  maybeRemovePolygon >>= 
                  maybeMovePolygon >>= 
                  mutatePolygons

-- |Add a polygon if it`s time to do so and the constraints are met
maybeAddPolygon :: DnaDrawing -> IO DnaDrawing
maybeAddPolygon d = maybeMutate activeAddPolygonMutationRate
                    (if (drawingPolygonsCount d < activePolygonsMax) then 
                         applyToPolygons addPolygon d else return d)
                    d
                    
-- |Remove a polygon if it`s time to do so and the constraints are met
maybeRemovePolygon :: DnaDrawing -> IO DnaDrawing
maybeRemovePolygon d = maybeMutate activeRemovePolygonMutationRate
                       (if (drawingPolygonsCount d > activePolygonsMin) then
                            applyToPolygons removePolygon d else return d)
                       d

-- |Move a polygon if it`s time to do so and the constraints are met
maybeMovePolygon :: DnaDrawing -> IO DnaDrawing
maybeMovePolygon d = maybeMutate activeMovePolygonMutationRate
                     (if (drawingPolygonsCount d > 0) then
                          applyToPolygons movePolygon d else return d)
                     d

-- |Mutate polygons if it`s time to do so and the constraints are met
mutatePolygons :: DnaDrawing -> IO DnaDrawing
mutatePolygons = applyToPolygons (mapM mutate)

-- |Remove a polygon at a random index
removePolygon :: [DnaPolygon] -> IO [DnaPolygon]
removePolygon p = do index <- getRandomNumber 0 (length p - 1)
                     return (removeElem index p)
                     
-- |Move a polygon in the list of polygons
movePolygon :: [DnaPolygon] -> IO [DnaPolygon]
movePolygon p = do from <- getRandomNumber 0 (length p - 1)
                   to   <- getRandomNumber 0 (length p - 1)
                   return (moveElem from to p)