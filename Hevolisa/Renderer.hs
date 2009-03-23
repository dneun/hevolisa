--
-- Module      : Renderer
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Renderer 
    ( drawingDelta
    , unpackSurface
    , drawingToFile
    , resizeDrawing
    ) where

import Control.Monad
import Data.ByteString ( unpack )
import qualified Graphics.Rendering.Cairo as C
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Shapes.DnaPolygon
import Hevolisa.Shapes.DnaBrush
import Hevolisa.Shapes.DnaPoint
import qualified Hevolisa.Settings as S
import System.FilePath ((</>), (<.>))
import System.IO.Error
import System.Directory

-- | Render the shapes with cairo
class Renderable a where
    render :: a -> C.Render ()

instance Renderable DnaPoint where
    render (DnaPoint x y) = C.lineTo (fromIntegral x) (fromIntegral y)

instance Renderable DnaPolygon where
    render p = do
      render $ brush p
      render $ points p
      C.fill

instance Renderable DnaBrush where
    render br = C.setSourceRGBA r g b a
        where r = normalize $ getRed br
              g = normalize $ getGreen br
              b = normalize $ getBlue br
              a = normalize $ getAlpha br
              normalize = (/255) . fromIntegral

instance Renderable DnaDrawing where
    render = render . polygons

instance (Renderable a) => Renderable [a] where
    render = mapM_ render

-- | Resize shapes
class Resizable a where
    resize :: Float -> a -> a

instance Resizable DnaPoint where
    resize f (DnaPoint x y) = DnaPoint (round $ (fromIntegral x) * f)
                                       (round $ (fromIntegral y) * f)

instance Resizable DnaPolygon where
    resize f p = p { points = map (resize f) $ points p }

instance Resizable DnaDrawing where
    resize f d = d { polygons = map (resize f) $ polygons d }

-- | 1. Rasterize the drawing
-- 2. Compare the color values of the drawing and the image pixel by
-- pixel
drawingDelta :: DnaDrawing -- ^ the drawing is rasterized
             -> Int -> Int -- ^ image width and height
             -> [Int]  -- ^ color values of the image
             -> IO Integer -- ^ return the color pixel error
drawingDelta drawing w h image = toSurface (render drawing) >>= 
                                 unpackSurface >>= 
                                 return . delta image
    where
      delta :: [Int] -> [Int] -> Integer
      delta a b = sum $ zipWith (\x y -> fromIntegral (x-y)^2) a b
                          
      toSurface :: C.Render () -> IO C.Surface
      toSurface r = do 
        surface <- C.createImageSurface C.FormatRGB24 w h
        C.renderWith surface r
        return surface

-- | Resize drawing both vertically and horizonally by given
-- proportion.
resizeDrawing :: Float -> DnaDrawing -> DnaDrawing
resizeDrawing = resize 

-- | Extract the color values to compute the error
unpackSurface :: C.Surface -> IO [Int]
unpackSurface s = C.imageSurfaceGetData s >>=
                  return . removeAlpha . map fromIntegral . unpack
    where
      -- remove the alpha channel
      removeAlpha :: [a] -> [a]
      removeAlpha []           = []
      removeAlpha (r:g:b:a:xs) = r:g:b:removeAlpha xs
      removeAlpha _            = Prelude.error "wrong number of color values"

-- | Rasterize the drawing and save it to a file
drawingToFile :: DnaDrawing -> Int -> Int -> Int -> IO ()
drawingToFile d w h n = do
  C.withImageSurface C.FormatRGB24 w h $ \surface -> do
                      C.renderWith surface $ render d
                      dirExists <- doesDirectoryExist subdir
                      unless dirExists $ createDirectory subdir
                      C.surfaceWriteToPNG surface filePath
    where filePath = subdir </> show n <.> "png"
          subdir = "images"
