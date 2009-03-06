--
-- Module      : DnaBrush
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Shapes.DnaBrush 
    ( DnaBrush
    , getRed, getGreen, getBlue, getAlpha
    ) where

import Data.Word ( Word8 )
import Hevolisa.Settings
import Hevolisa.Tools

-- | Brush to color polygons
data DnaBrush = DnaBrush 
    { getRed   :: Integer
    , getGreen :: Integer
    , getBlue  :: Integer
    , getAlpha :: Integer
    } deriving (Show, Eq, Read)

-- | Brush is mutable
instance Mutable DnaBrush where
    mutate = mutateBrush

-- | Initialize brush with random garbage
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
    where
      -- |Mutate a one-dimensional value
      maybeMutate_ :: Integer    -- ^ The unchanged value to pass through
                   -> Integer    -- ^ Mutation rate
                   -> Integer    -- ^ Minimum for random numbers
                   -> Integer    -- ^ Maximum for random numbers
                   -> IO Integer -- ^ Changed value
      maybeMutate_ unchanged rate min max = maybeMutate rate (getRandomNumber min max) unchanged