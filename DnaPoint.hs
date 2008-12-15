module DnaPoint where

import Settings
import Tools

data DnaPoint = DnaPoint Integer Integer deriving (Show,Eq)

pointX :: DnaPoint -> Integer
pointX (DnaPoint x _) = x

pointY :: DnaPoint -> Integer
pointY (DnaPoint _ y) = y

instance Mutable DnaPoint where
    mutate = mutatePoint

initPoint :: IO DnaPoint
initPoint = do x <- getRandomNumber 0 maxWidth
               y <- getRandomNumber 0 maxHeight
               return (DnaPoint x y)

mutatePoint :: DnaPoint -> IO DnaPoint
mutatePoint p = mutateMax p >>= mutateMid >>= mutateMin
    where mutateMax   = maybeMutate activeMovePointMaxMutationRate initPoint
          mutateMid p = maybeMutate activeMovePointMidMutationRate 
                        (pointFunction midX midY p) p
          mutateMin p = maybeMutate activeMovePointMinMutationRate 
                        (pointFunction minX minY p) p

pointFunction :: (Integer -> IO Integer) -> (Integer -> IO Integer) -> DnaPoint -> IO DnaPoint
pointFunction xFunction yFunction p = do x <- xFunction $ pointX p
                                         y <- yFunction $ pointY p
                                         return (DnaPoint x y)

midX, midY, minX, minY :: Integer -> IO Integer
midX = mutateDim activeMovePointRangeMid maxWidth
midY = mutateDim activeMovePointRangeMid maxHeight
minX = mutateDim activeMovePointRangeMin maxWidth
minY = mutateDim activeMovePointRangeMin maxHeight

mutateDim :: Integer -> Integer -> Integer -> IO Integer
mutateDim range maxn n = do random <- getRandomNumber (-range) range
                            return (min (max 0 (n + random)) maxn)