module Tools (
              Mutable,
              willMutate,
              maybeMutate,
              getRandomNumber
) where

import Random

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

-- |Perform a mutation action when it`s time to do so
maybeMutate :: Integer  -- ^ Mutation rate
            -> IO a     -- ^ Mutation action
            -> a        -- ^ Unchanged value to pass through
            -> IO a     -- ^ Composed action
maybeMutate rate action unchanged = do mutate <- willMutate rate
                                       if mutate then action else return unchanged

-- |Instances of Mutable can mutate their DNA
class Mutable a where
    mutate :: a -> IO a