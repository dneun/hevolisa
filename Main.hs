module Main where

import DnaDrawing
import Evolution
import Tools

-- main :: IO ()
-- main = do drawing <- mapseq initDrawing $ replicate 100000 mutate
--           putStrLn $ show drawing

main :: IO ()
main = mapseq (initContext [[]]) (replicate 10 mutate) >>= \c -> 
       (putStrLn . show) c