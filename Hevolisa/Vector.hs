--
-- Module      : Hevolisa.Vector
-- Copyright   : (c) Daniel Neun 2009
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

{-# LANGUAGE PArr #-}
{-# OPTIONS -fvectorise #-}
module Hevolisa.Vector where

import qualified Prelude as P
import Data.Array.Parallel.Prelude
import Data.Array.Parallel.Prelude.Int

error :: [:Int:] -> [:Int:] -> Int
error c1 c2 = sumP (zipWithP (\x y -> (x - y)*(x - y)) c1 c2)

error_wrapper :: PArray Int -> PArray Int -> Int
{-# NOINLINE error_wrapper #-}
error_wrapper c1 c2 = error (fromPArrayP c1) (fromPArrayP c2)