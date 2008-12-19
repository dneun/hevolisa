module Main where

import DnaPoint
import DnaBrush
import DnaPolygon
import DnaDrawing
import Evolution
import Tools

main :: IO ()
main = do (mapseq randomInit $ replicate 1000 mutate :: IO DnaDrawing) >>= putStrLn . show
          (randomInit >>= mutate >>= mutate :: IO DnaPoint) >>= putStrLn . show
          (randomInit >>= mutate >>= mutate :: IO DnaBrush) >>= putStrLn . show
          (randomInit >>= mutate >>= mutate :: IO DnaPolygon) >>= putStrLn . show
          (randomInit >>= mutate >>= mutate :: IO DnaDrawing) >>= putStrLn . show
          