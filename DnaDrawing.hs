-- |Intended usage: @ initDrawing >>= mutate >>= mutate >>= mutate @
module DnaDrawing (
                   DnaDrawing,
                   mapseq
) where

import DnaPolygon ( DnaPolygon )
import Tools
import Settings

-- |A drawing contains an ordered set of polygons
data DnaDrawing = DnaDrawing {
      polygons :: [DnaPolygon] 
} deriving (Show,Eq,Read)

-- |Get the number of polygons of a drawing to check constraints
polygonsCount :: Integral a => DnaDrawing -> a
polygonsCount = fromIntegral . length . polygons

instance Points DnaDrawing where
    pointCount = sum . map pointCount . polygons

-- |Sequences functions that produce actions
mapseq :: Monad m => m a -> [a -> m a] -> m a
mapseq f []     = f
mapseq s (f:fs) = mapseq (s >>= f) fs

-- |Construct and init a new Drawing
instance RandomInit DnaDrawing where
    randomInit = mseq (replicate min addPolygon) >>= return . DnaDrawing
        where min = fromIntegral activePolygonsMin
              mseq = mapseq (return [])

-- |Add a new polygon at a random position
addPolygon :: [DnaPolygon] -> IO [DnaPolygon]
addPolygon ps = do random <- getRandomNumber 0 (length ps)
                   polygon <- randomInit
                   return (addElem polygon random ps)

-- |Drawing has mutable DNA
instance Mutable DnaDrawing where
    mutate = mutateDrawing

-- |Apply a polygon function to a drawing
applyToPolygons :: ([DnaPolygon] -> IO [DnaPolygon])
                -> DnaDrawing
                -> IO DnaDrawing
applyToPolygons f d = f (polygons d) >>= return . DnaDrawing

-- |Basic drawing mutation function
mutateDrawing :: DnaDrawing -> IO DnaDrawing
mutateDrawing d = maybeAddPolygon d >>= 
                  maybeRemovePolygon >>= 
                  maybeMovePolygon >>= 
                  mutatePolygons

-- |Add a polygon if it`s time to do so and the constraints are met
maybeAddPolygon :: DnaDrawing -> IO DnaDrawing
maybeAddPolygon d = maybeMutate activeAddPolygonMutationRate
                    (if (polygonsCount d < activePolygonsMax) then 
                         applyToPolygons addPolygon d else return d)
                    d
                    
-- |Remove a polygon if it`s time to do so and the constraints are met
maybeRemovePolygon :: DnaDrawing -> IO DnaDrawing
maybeRemovePolygon d = maybeMutate activeRemovePolygonMutationRate
                       (if (polygonsCount d > activePolygonsMin) then
                            applyToPolygons removePolygon d else return d)
                       d

-- |Move a polygon if it`s time to do so and the constraints are met
maybeMovePolygon :: DnaDrawing -> IO DnaDrawing
maybeMovePolygon d = maybeMutate activeMovePolygonMutationRate
                     (if (polygonsCount d > 0) then
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