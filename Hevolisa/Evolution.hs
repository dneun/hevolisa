--
-- Module      : Evolution
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Evolution where

import Data.Word
import Prelude hiding (error)
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Tools
import Hevolisa.Renderer ( drawingError )
import Debug.Trace

-- |Context contains the current drawing and the source image for comparison
data EvolutionContext = EvolutionContext {
      drawing :: DnaDrawing,
      image   :: FilePath
}  deriving (Show, Eq)

generations = 10000

-- |Init the context with image and initial drawing
initContext :: FilePath  -> IO EvolutionContext
initContext s = randomInit >>= return . flip EvolutionContext s

-- |Start the evolution process
start :: FilePath -> IO EvolutionContext
start s = foldl (>>=) (initContext s) (replicate generations step)

-- |Color error, smaller is better
error :: EvolutionContext -> IO (Maybe Word8)
error (EvolutionContext drawing source) = drawingError drawing source

-- |EvolutionContext mutates minimizing the error
instance Mutable EvolutionContext where
    mutate = step

-- |Single evolution step, minimize error
step :: EvolutionContext -> IO EvolutionContext
step ec = do next <- mutateEvolutionContext ec
             e1 <- error ec
             e2 <- error next
             if e1 < e2 then trace (show e1) return ec else trace (show e2) return next

-- |Mutate the drawing in the EvolutionContext
mutateEvolutionContext :: EvolutionContext -> IO EvolutionContext
mutateEvolutionContext (EvolutionContext d s) = do m <- mutate d
                                                   return (EvolutionContext m s)