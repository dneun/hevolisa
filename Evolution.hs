module Evolution where

import Prelude hiding (error)
import DnaDrawing
import Tools

generations = 10000

start = mapseq initDrawing (replicate generations step)

-- |Smaller is better
error :: DnaDrawing -> Double
error = undefined

-- |Single evloution step
step :: DnaDrawing -> IO DnaDrawing
step d = mutate d >>= \next -> return (min d next)

-- |Compare Drawings by fitness
instance Ord DnaDrawing where
    compare x y
        | error x == error y = EQ
        | error x <= error y = LT
        | otherwise          = GT

-- TODO: Maybe step, error and Ord instance should be defined on some
-- kind of context which contains the image which is used to compute
-- the error
--
-- The error function must have the image as a parameter