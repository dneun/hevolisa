--
-- Module      : ColorMatrix
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

{-# OPTIONS_GHC -XMultiParamTypeClasses -XFlexibleContexts #-}
module ColorMatrix (
                    ColorMatrix,

                    imageError,
                    readImage,
                    renderDrawing
                   ) where

import DnaDrawing ( DnaDrawing )

type Matrix a = [[a]]
-- |Contains the color pixels of an image
type ColorMatrix = Matrix Color
data Color = Color (Double,Double,Double) deriving (Show,Eq)
type Filename = String

-- |General color class
class RGBColor c r where
    red   :: c -> r
    green :: c -> r
    blue  :: c -> r

instance RGBColor Color Double where
    red   (Color (r,_,_)) = r
    green (Color (_,g,_)) = g
    blue  (Color (_,_,b)) = b

-- |Get the color error of two colors
colorError :: (RGBColor a b,Num b) => a -> a -> b
colorError c1 c2 = let deltaRed   = red c1 - red c2
                       deltaGreen = green c1 - green c2
                       deltaBlue  = blue c1 - blue c2
                   in deltaRed * deltaRed +
                      deltaGreen * deltaGreen +
                      deltaBlue * deltaBlue

-- |Get the color error of two images
imageError :: (Num b,RGBColor Color b) => ColorMatrix -> ColorMatrix -> b
imageError cm1 cm2 = sum $ zipWith colorError (concat cm1) (concat cm2)

-- |Read an image from a file
readImage :: Filename -> IO ColorMatrix
readImage = undefined

-- |Render a drawing
renderDrawing :: DnaDrawing -> IO ColorMatrix
renderDrawing = undefined