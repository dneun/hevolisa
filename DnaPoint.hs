module DnaPoint (
                 DnaPoint (DnaPoint),

                 -- Accessors
                 pointX,
                 pointY,

                 -- Constructors, mutate
                 initPoint,
                 mutatePoint,
                 randomPoint
                ) where

import Settings
import Tools

-- |Mutable Point
data DnaPoint = DnaPoint Integer Integer deriving (Show,Eq)

-- |X value of the point
pointX :: DnaPoint -> Integer
pointX (DnaPoint x _) = x

-- |Y value of the point
pointY :: DnaPoint -> Integer
pointY (DnaPoint _ y) = y

-- |DnaPoint is mutable
instance Mutable DnaPoint where
    mutate = mutatePoint

-- |Initialize point with random garbage
initPoint :: IO DnaPoint
initPoint = do x <- getRandomNumber 0 maxWidth
               y <- getRandomNumber 0 maxHeight
               return (DnaPoint x y)

-- |Mutate points dna randomly, rates can be adjusted
mutatePoint :: DnaPoint -> IO DnaPoint
mutatePoint p = mutateMax p >>= mutateMid >>= mutateMin
    where mutateMax   = maybeMutate activeMovePointMaxMutationRate initPoint
          mutateMid p = maybeMutate activeMovePointMidMutationRate 
                        (pointFunction midX midY p) p
          mutateMin p = maybeMutate activeMovePointMinMutationRate 
                        (pointFunction minX minY p) p

-- |Change the x and y values of the point with functions
pointFunction :: (Integer -> IO Integer) -- ^ Function to change the x value
              -> (Integer -> IO Integer) -- ^ Function to change the y value
              -> DnaPoint                -- ^ Original point
              -> IO DnaPoint             -- ^ Changed point (action)
pointFunction xFunction yFunction p = do x <- xFunction $ pointX p
                                         y <- yFunction $ pointY p
                                         return (DnaPoint x y)

-- |Helper functions for different ranges
midX, midY, minX, minY :: Integer -> IO Integer
midX = mutateDim activeMovePointRangeMid maxWidth
midY = mutateDim activeMovePointRangeMid maxHeight
minX = mutateDim activeMovePointRangeMin maxWidth
minY = mutateDim activeMovePointRangeMin maxHeight

-- |Mutate a one-dimensional value
mutateDim :: Integer    -- ^ Randomisation range
          -> Integer    -- ^ Maximum
          -> Integer    -- ^ Original value
          -> IO Integer -- ^ New value (action)
mutateDim range maxn n = do random <- getRandomNumber (-range) range
                            return (min (max 0 (n + random)) maxn)

-- |Create a random point using another point
randomPoint :: DnaPoint -> IO DnaPoint
randomPoint (DnaPoint x y) = do x <- mutateDim 3 maxWidth x
                                y <- mutateDim 3 maxHeight y
                                return (DnaPoint x y)