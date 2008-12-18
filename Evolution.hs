module Evolution where

import Prelude hiding (error)
import DnaDrawing
import Tools
import ColorMatrix ( ColorMatrix, imageError, renderDrawing )

-- |Context contains the current drawing and the source image for comparison
data EvolutionContext = EvolutionContext DnaDrawing ColorMatrix
                        deriving (Show,Eq)

generations = 10000

-- |Init the context with source image and initial drawing
initContext :: ColorMatrix -> IO EvolutionContext
initContext s = initDrawing >>= \d -> return (EvolutionContext d s)

-- |Start the evolution process
start :: ColorMatrix -> IO EvolutionContext
start s = mapseq (initContext s) (replicate generations step)

-- |Color error, smaller is better
error :: EvolutionContext -> IO Double
error (EvolutionContext drawing source) = renderDrawing drawing >>= \d ->
                                          return (imageError d source)

-- |EvolutionContext mutates minimizing the error
instance Mutable EvolutionContext where
    mutate = step

-- |Single evolution step, minimize error
step :: EvolutionContext -> IO EvolutionContext
step ec = do next <- mutateEvolutionContext ec
             e1 <- error ec
             e2 <- error next
             if e1 < e2 then return ec else return next

-- |Mutate the drawing in the EvolutionContext
mutateEvolutionContext :: EvolutionContext -> IO EvolutionContext
mutateEvolutionContext (EvolutionContext d s) = do m <- mutate d
                                                   return (EvolutionContext m s)