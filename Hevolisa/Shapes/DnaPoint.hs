--
-- Module      : DnaPoint
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Shapes.DnaPoint (
                 DnaPoint (DnaPoint),

                 -- * Accessors
                 pointX,
                 pointY,

                 -- * Constructors, mutate
                 randomPoint
                ) where

import Data.IORef
import Hevolisa.Settings
import Hevolisa.Tools

-- |Mutable Point
data DnaPoint = DnaPoint {
      pointX :: Double,
      pointY :: Double 
} deriving (Show,Eq,Read)

-- |DnaPoint is mutable
instance Mutable DnaPoint where
    mutate = mutatePoint

-- |Initialize point with random garbage
instance RandomInit DnaPoint where
    randomInit = do
      [mw, mh] <- mapM readIORef [maxWidth, maxHeight]
      x <- getRandomNumber 0.0 mw
      y <- getRandomNumber 0.0 mh
      return (DnaPoint x y)

-- |Mutate points dna randomly, rates can be adjusted
mutatePoint :: DnaPoint -> IO DnaPoint
mutatePoint p = mutateMax p >>= mutateMid >>= mutateMin
    where mutateMax   = maybeMutate activeMovePointMaxMutationRate randomInit
          mutateMid p = maybeMutate activeMovePointMidMutationRate 
                        (pointFunction midP midP p) p
          mutateMin p = maybeMutate activeMovePointMinMutationRate 
                        (pointFunction minP minP p) p

          -- |Change the x and y values of the point with functions
          pointFunction :: (Double -> Double -> IO Double) -- ^ Function to change the x value
                        -> (Double -> Double -> IO Double) -- ^ Function to change the y value
                        -> DnaPoint              -- ^ Original point
                        -> IO DnaPoint           -- ^ Changed point (action)
          pointFunction fx fy p = do 
              mw <- readIORef maxWidth 
              x <- fx mw $ pointX p
              mh <- readIORef maxHeight
              y <- fy mh $ pointY p
              return (DnaPoint x y)

          -- |Helper functions for different ranges
          midP, minP :: Double -> Double -> IO Double
          midP = mutateDim activeMovePointRangeMid
          minP = mutateDim activeMovePointRangeMin

-- |Mutate a one-dimensional value
mutateDim :: Double    -- ^ Randomisation range
          -> Double    -- ^ Maximum
          -> Double    -- ^ Original value
          -> IO Double -- ^ New value (action)
mutateDim range maxn n = getRandomNumber (-range) range >>=
                         return . min maxn . max 0 . (+ n)

-- |Create a random point using another point
randomPoint :: DnaPoint -> IO DnaPoint
randomPoint (DnaPoint x y) = do 
      mw <- readIORef maxWidth
      x <- mutateDim 3 mw x
      mh <- readIORef maxHeight
      y <- mutateDim 3 mh y
      return (DnaPoint x y)