module Evolution where

import Prelude hiding (error)
import DnaDrawing
import Tools

generations = 10000

start = mapseq initDrawing (replicate generations evolve)

-- |Smaller is better
error :: DnaDrawing -> Double
error = undefined

-- |Single evloution step
evolve :: DnaDrawing -> IO DnaDrawing
evolve d = mutate d >>= \next -> return (min d next)

-- |Compare Drawings by fitness
instance Ord DnaDrawing where
    compare x y
        | error x == error y = EQ
        | error x <= error y = LT
        | otherwise          = GT