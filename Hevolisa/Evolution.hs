--
-- Module      : Evolution
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Evolution where

import qualified Data.Array.Parallel.Prelude as P
import Prelude hiding (error)
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Tools
import Hevolisa.Renderer ( drawingError,drawingToFile,withImageFromPNG)
import Debug.Trace

-- |Context contains the current drawing and the source image for comparison
data EvolutionContext = EvolutionContext {
      drawing :: DnaDrawing,
      image   :: P.PArray Int
} deriving (Show)

type Error = Int

-- |Init the context with image and initial drawing
initContext :: P.PArray Int  -> IO EvolutionContext
initContext image = randomInit >>= \drawing ->
                    return $ EvolutionContext drawing image

-- |Number of mutations between image writes
imageInterval = 100

-- |Start the evolution process
start :: FilePath -> IO (EvolutionContext,Error)
start fp = do c <- withImageFromPNG fp initContext 
              e <- error c
              iter 0 (c,e)

-- |Recursive function combines mutation and writing files
iter :: Int -> (EvolutionContext,Error) -> IO (EvolutionContext,Error)
iter n (c1,e1) = do maybeWriteToFile c1
                    c2 <- mutate c1
                    e2 <- error c2
                    iter (n + 1) $ minError (c1,e1)(c2,e2)
    where maybeWriteToFile
              | isTimeToWrite = \ec -> drawingToFile (drawing ec) n >> return ec
              | otherwise     = return
          isTimeToWrite = n `mod` imageInterval == 0
          minError (c1,e1)(c2,e2) | e1 < e2   = trace (show e1) $ (c1,e1)
                                  | otherwise = trace (show e2) $ (c2,e2)

-- |Color error, smaller is better
error :: EvolutionContext -> IO Int
error (EvolutionContext drawing source) = drawingError drawing source

-- |EvolutionContext mutates minimizing the error
instance Mutable EvolutionContext where
    mutate = mutateEvolutionContext

-- |Mutate the drawing in the EvolutionContext
mutateEvolutionContext :: EvolutionContext -> IO EvolutionContext
mutateEvolutionContext (EvolutionContext d s) = do m <- mutate d
                                                   return (EvolutionContext m s)