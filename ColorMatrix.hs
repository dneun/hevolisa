module ColorMatrix (
                    ColorMatrix,

                    imageError,
                    readImage,
                    renderDrawing
                   ) where

import DnaDrawing

type Matrix a = [[a]]
type ColorMatrix = Matrix Color
data Color = Color (Double,Double,Double)
type Filename = String

class RGBColor c where
    red   :: c -> Double
    green :: c -> Double
    blue  :: c -> Double

instance RGBColor Color where
    red   (Color (r,_,_)) = r
    green (Color (_,g,_)) = g
    blue  (Color (_,_,b)) = b

colorError :: (RGBColor a) => a -> a -> Double
colorError c1 c2 = let deltaRed   = red c1 - red c2
                       deltaGreen = green c1 - green c2
                       deltaBlue  = blue c1 - blue c2
                   in deltaRed * deltaRed +
                      deltaGreen * deltaGreen +
                      deltaBlue * deltaBlue

imageError :: ColorMatrix -> ColorMatrix -> Double
imageError cm1 cm2 = sum $ zipWith colorError (concat cm1) (concat cm2)

-- |Read an image from a file
readImage :: Filename -> ColorMatrix
readImage = undefined

-- |Render a drawing
renderDrawing :: DnaDrawing -> ColorMatrix
renderDrawing = undefined