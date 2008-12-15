module Tools where

import Random

willMutate :: Integer -> IO Bool
willMutate mutationRate = do k <- getRandomNumber 0 mutationRate
                             return (k == 1)

getRandomNumber :: Random a => a -> a -> IO a
getRandomNumber x y = getStdRandom (randomR (x,y))

maybeMutate :: Integer -> IO a -> a -> IO a
maybeMutate rate action unchanged = do mutate <- willMutate rate
                                       if mutate then action else return unchanged

class Mutable a where
    mutate :: a -> IO a

maxPolygons = 250
maxWidth = 200
maxHeight = 200