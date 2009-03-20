--
-- Module      : Evolution
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Evolution where

import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Tools
import Hevolisa.Renderer ( drawingDelta, drawingToFile, withImageFromPNG )

-- | Context contains the current drawing and the source image for comparison
data EvolutionContext = EvolutionContext {
      drawing :: DnaDrawing,
      image   :: [Int]
} deriving (Show, Eq)

type Delta = Integer

-- | Init the context with image and initial drawing
initContext :: [Int]  -> IO EvolutionContext
initContext image = do
  drawing <- randomInit
  return $ EvolutionContext drawing image

-- | Number of mutations between image writes
imageInterval = 1000

-- | Start the evolution process
evolve :: FilePath -> IO (EvolutionContext, Delta)
evolve fp = do 
  c <- withImageFromPNG fp initContext 
  e <- calculateDelta c
  iter 0 (c,e)

-- | Recursive function combines mutation and writing files
iter :: Int -> (EvolutionContext, Delta) -> IO (EvolutionContext, Delta)
iter n (c,e) = do 
  print e
  maybeWriteToFile c
  c' <- mutate c
  e' <- calculateDelta c'
  iter (n + 1) $ if e' < e then (c',e') else (c, e)
    where 
      maybeWriteToFile
          | isTimeToWrite = \ec -> drawingToFile (drawing ec) n >> return ec
          | otherwise     = return
      isTimeToWrite = n `mod` imageInterval == 0

-- | Color error, smaller is better
calculateDelta :: EvolutionContext -> IO Integer
calculateDelta (EvolutionContext drawing source) = drawingDelta drawing source

-- | EvolutionContext mutates minimizing the error
instance Mutable EvolutionContext where
    mutate = mutateEvolutionContext

-- | Mutate the drawing in the EvolutionContext
mutateEvolutionContext :: EvolutionContext -> IO EvolutionContext
mutateEvolutionContext (EvolutionContext d s) = do m <- mutate d
                                                   return (EvolutionContext m s)
