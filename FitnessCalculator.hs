module FitnessCalculator (
                          Image
                         ,Color
                         ,Error
                         ,colorFitness
                         ,error_
                         ) where

import Random (getStdRandom,randomR)

type Image = [Color]
type Color = (Double,Double,Double)
type Error = Double

-- | Measure the distance between the colors in 3D space
colorFitness :: Color -> Color -> Error
colorFitness (r1,g1,b1) (r2,g2,b2) = let deltaRed   = r1 - r2
                                         deltaGreen = g1 - g2
                                         deltaBlue  = b1 - b2
                                     in deltaRed * deltaRed +
                                        deltaBlue * deltaBlue +
                                        deltaGreen * deltaGreen

-- | Combine the pixel fitness to the total fitness ( lower is better )
error_ :: Image -> Image -> Error
error_ a b = sum $ zipWith colorFitness a b

-- | Generate a randomized color
randomColor :: IO Color
randomColor = do r <- random
                 g <- random
                 b <- random
                 return (r,g,b)
                     where random = getStdRandom (randomR (0,255))

-- | Create an image with random colors
randomImage :: Int -> IO Image
randomImage n = sequence $ replicate n randomColor
--randomImage = sequence . flip replicate randomPixel

-- | Create two random color images and compute the fitness
randomFitness :: Int -> IO Error
randomFitness n = do i1 <- randomImage n
                     i2 <- randomImage n
                     return (error_ i1 i2)


try :: Int -> IO [Error]
try n = sequence $ replicate n $ randomFitness 10

minimize :: IO [Error] -> IO [Error]
minimize = fmap (scanl1 min)

minimal :: IO [Error] -> IO Error
minimal = fmap (foldr1 min)