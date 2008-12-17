module Main where

import DnaDrawing
import Tools

main :: IO ()
main = do drawing <- mapseq initDrawing $ replicate 100000 mutate
          putStrLn $ show drawing