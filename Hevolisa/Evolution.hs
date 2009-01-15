--
-- Module      : Evolution
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Evolution where

import Prelude hiding (error)
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Tools
import Hevolisa.Renderer ( drawingError,drawingToFile,withImageFromPNG)
import Debug.Trace

-- |Context contains the current drawing and the source image for comparison
data EvolutionContext = EvolutionContext {
      drawing :: DnaDrawing,
      image   :: [Integer]
} deriving (Show, Eq)

generations = 4000

-- |Init the context with image and initial drawing
initContext :: [Integer]  -> IO EvolutionContext
initContext s = randomInit >>= return . flip EvolutionContext s

-- |Number of mutations between image writes
imageInterval = 100

-- |Start the evolution process
start :: FilePath -> IO EvolutionContext
start fp = init >>= (iter 0)
    where iter :: Int -> EvolutionContext -> IO EvolutionContext
          iter n ec = do  { ec <- mutate ec;
                            if (n `mod` imageInterval == 0)
                            then drawingToFile (drawing ec) n
                            else return ();
                             iter (n + 1) ec; }
          init = withImageFromPNG fp initContext

-- |Color error, smaller is better
error :: EvolutionContext -> IO Integer
error (EvolutionContext drawing source) = drawingError drawing source

-- |EvolutionContext mutates minimizing the error
instance Mutable EvolutionContext where
    mutate c = do (context,error) <- step c
                  trace (show error) $ return context

-- |Single evolution step, minimize error
step :: EvolutionContext -> IO (EvolutionContext,Integer)
step ec = do next <- mutateEvolutionContext ec
             e1 <- error ec
             e2 <- error next
             if e1 < e2 then return (ec,e1) else return (next,e2)

-- |Mutate the drawing in the EvolutionContext
mutateEvolutionContext :: EvolutionContext -> IO EvolutionContext
mutateEvolutionContext (EvolutionContext d s) = do m <- mutate d
                                                   return (EvolutionContext m s)