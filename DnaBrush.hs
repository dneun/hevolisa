module DnaBrush (
                 DnaBrush,
                         
                 -- Accessors
                 red, green, blue, alpha
) where

import Settings
import Tools

-- |Brush to color polygons
data DnaBrush = DnaBrush {
      red   :: Integer,
      green :: Integer,
      blue  :: Integer,
      alpha :: Integer 
} deriving (Show,Eq,Read)

-- |Brush is mutable
instance Mutable DnaBrush where
    mutate = mutateBrush

-- |Initialize brush with random garbage
instance RandomInit DnaBrush where
    randomInit = do r <- getRandomNumber 0 255
                    g <- getRandomNumber 0 255
                    b <- getRandomNumber 0 255
                    a <- getRandomNumber 10 60
                    return (DnaBrush r g b a)

-- |Change the brush values in a semi random way, rates can be adjusted
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

-- |Mutate a one-dimensional value
maybeMutate_ :: Integer    -- ^ The unchanged value to pass through
             -> Integer    -- ^ Mutation rate
             -> Integer    -- ^ Minimum for random numbers
             -> Integer    -- ^ Maximum for random numbers
             -> IO Integer -- ^ Changed value
maybeMutate_ unchanged rate min max = maybeMutate rate (getRandomNumber min max) unchanged