module Tools (
              willMutate
             ,getRandomNumber
             ,maxPolygons
             ,maxWidth
             ,maxHeight)
where

import Random

willMutate :: Integer -> IO Bool
willMutate mutationRate = do k <- getRandomNumber 0 mutationRate
                             return (k == 1)

getRandomNumber :: Random a => a -> a -> IO a
getRandomNumber x y = getStdRandom (randomR (x,y))

maxPolygons = 250
maxWidth = 200
maxHeight = 200