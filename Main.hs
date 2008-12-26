--
-- Module      : Main
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Main where

import DnaDrawing
import Tools (randomInit, mutate)
import Renderer
import Evolution (start)

generations = 10000

-- main :: IO ()
-- main = let lst = replicate generations mutate
--        in (foldl (>>=) randomInit lst :: IO DnaDrawing) >>= display

main :: IO ()
main = start "mona_lisa_crop.png" >>= putStrLn . show