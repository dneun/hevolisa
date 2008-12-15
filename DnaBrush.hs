module DnaBrush where

import Settings
import Tools

data DnaBrush = DnaBrush Integer Integer Integer Integer deriving (Show)

instance Mutable DnaBrush where
    mutate = mutateBrush

initBrush :: IO DnaBrush
initBrush = do r <- getRandomNumber 0 255
               g <- getRandomNumber 0 255
               b <- getRandomNumber 0 255
               a <- getRandomNumber 10 60
               return (DnaBrush r g b a)

mutateBrush :: DnaBrush -> IO DnaBrush
mutateBrush (DnaBrush r g b a) = do r <- maybeMutate_ r activeRedMutationRate 
                                         activeRedRangeMin activeRedRangeMax
                                    g <- maybeMutate_ g activeGreenMutationRate 
                                         activeGreenRangeMin activeGreenRangeMax
                                    b <- maybeMutate_ b activeBlueMutationRate 
                                         activeBlueRangeMin activeBlueRangeMax
                                    a <- maybeMutate_ a activeAlphaMutationRate 
                                         activeAlphaRangeMin activeAlphaRangeMax
                                    return (DnaBrush r g b a)

maybeMutate_ :: Integer -> Integer -> Integer -> Integer -> IO Integer
maybeMutate_ unchanged rate min max = maybeMutate rate (getRandomNumber min max) unchanged