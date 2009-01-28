--
-- Module      : Tools
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Tools (
              -- * Classes
              Mutable (mutate),
              Points (pointCount),
              RandomInit (randomInit),

              -- * Utility functions
              when,
              willMutate,
              maybeMutate,
              getRandomNumber,

              -- * List index functions
              addElem,
              removeElem,
              moveElem
) where

import Random

-- |Instances of Mutable can mutate their DNA
class Mutable a where
    -- |Perform a genetic mutation in a random way
    mutate :: a -> IO a

-- |Count the points
class Points a where
    -- |Count the points
    pointCount :: (Integral b) => a -> b

-- |Initialize a shape with random values
class RandomInit a where
    -- |Action that returns a shape with random values
    randomInit :: IO a

-- |Decide whether it`s time for a mutation
willMutate :: Integer  -- ^ Mutation rate 
           -> IO Bool  -- ^ True: Mutate
willMutate mutationRate = do k <- getRandomNumber 0 mutationRate
                             return (k == 1)

-- |Get a random number between min and max
getRandomNumber :: Random a => 
                   a     -- ^ Minimum
                -> a     -- ^ Maximum
                -> IO a  -- ^ Random number action
getRandomNumber x y = getStdRandom (randomR (x,y))

-- |Helper function for control of an action
when :: Bool -> (a -> IO a) -> a -> Bool -> IO a
when constraint f = flip (awhen constraint f)
    where awhen :: Bool -> (a -> IO a) -> Bool -> (a -> IO a)
          awhen constraint f mutate | mutate && constraint = f
                                    | otherwise            = return

-- |Perform a mutation action when it`s time to do so
maybeMutate :: Integer  -- ^ Mutation rate
            -> IO a     -- ^ Mutation action
            -> a        -- ^ Unchanged value to pass through
            -> IO a     -- ^ Composed action
maybeMutate rate action unchanged = do mutate <- willMutate rate
                                       if mutate then action else return unchanged

-- |Move a list element from index to index
moveElem :: Int -- ^ from index
         -> Int -- ^ to index
         -> [a] -- ^ original list 
         -> [a] -- ^ result
moveElem from to lst = addElem elem to $ removeElem from lst
    where elem = lst !! from


-- |Remove an item at index position from list
removeElem :: Int -- ^ index
           -> [a] -- ^ original list
           -> [a] -- ^ result
removeElem n lst = left ++ right
    where left = take n lst
          right = drop (n + 1) lst

-- |Add an item at index position to list
addElem :: a   -- ^ element
        -> Int -- ^ index
        -> [a] -- ^ original list
        -> [a] -- ^ result
addElem item index lst = left ++ [item] ++ right
    where left = take index lst
          right = drop index lst