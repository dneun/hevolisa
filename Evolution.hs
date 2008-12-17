module Evolution where

import Prelude hiding (error)
import DnaDrawing
import Tools

data EvolutionContext = EvolutionContext DnaDrawing SourceColorMatrix
                        deriving (Show,Eq)
data SourceColorMatrix = SourceColorMatrix deriving (Show,Eq)

generations = 10000

initContext :: SourceColorMatrix -> IO EvolutionContext
initContext s = initDrawing >>= \d -> return (EvolutionContext d s)

start s = mapseq (initContext s) (replicate generations step)

-- |Smaller is better
error :: EvolutionContext -> Double
error = undefined

-- |Single evolution step
step :: EvolutionContext -> IO EvolutionContext
step ec = mutate ec >>= \next -> return (min ec next)

instance Mutable EvolutionContext where
    mutate (EvolutionContext d s) = do m <- mutate d
                                       return (EvolutionContext m s)

-- |Compare EvolutionContext by error
instance Ord EvolutionContext where
    compare x y
        | error x == error y = EQ
        | error x <= error y = LT
        | otherwise          = GT