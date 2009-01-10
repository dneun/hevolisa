--
-- Module      : Main
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Main where

import Hevolisa.Evolution (start)


main :: IO ()
main = start "mona_lisa_crop.png" >> return ()