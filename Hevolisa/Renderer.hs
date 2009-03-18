--
-- Module      : Renderer
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Hevolisa.Renderer 
    ( drawingDelta
    , withImageFromPNG
    , drawingToFile
    ) where

import System.FilePath ((</>), (<.>))
import System.IO.Error
import Control.Monad
import Data.ByteString (unpack)
import Data.IORef
import Directory
import qualified Graphics.Rendering.Cairo as C
import Hevolisa.Shapes.DnaDrawing
import Hevolisa.Shapes.DnaPolygon
import Hevolisa.Shapes.DnaBrush
import Hevolisa.Shapes.DnaPoint
import qualified Hevolisa.Settings as S

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


-- | 1. Rasterize the drawing
-- 2. Compare the color values of the drawing and the image pixel by pixel
drawingDelta :: DnaDrawing -- ^ the drawing is rasterized
             -> [Integer]  -- ^ color values of the image
             -> IO Integer -- ^ return the color pixel error
drawingDelta drawing image = toSurface (render drawing) >>= 
                             unpackSurface >>= 
                             return . delta image
    where
      delta :: [Integer] -> [Integer] -> Integer
      delta a b = sum $ zipWith (\x y -> (x-y)^2) a b
                          
      toSurface :: C.Render () -> IO C.Surface
      toSurface r = do 
        [mw, mh] <- mapM readIORef [S.maxWidth, S.maxHeight]
        surface <- C.createImageSurface C.FormatRGB24 mw mh
        C.renderWith surface r
        return surface

-- | Extract the color values to compute the error
unpackSurface :: C.Surface -> IO [Integer]
unpackSurface s = C.imageSurfaceGetData s >>=
                  return . removeAlpha . map fromIntegral . unpack
    where
      -- remove the alpha channel
      removeAlpha :: [a] -> [a]
      removeAlpha []           = []
      removeAlpha (r:g:b:a:xs) = r:g:b:removeAlpha xs
      removeAlpha _            = Prelude.error "wrong number of color values"

-- | Open an image file and apply the function
withImageFromPNG :: FilePath -> ([Integer] -> IO a) -> IO a
withImageFromPNG fp f = do
  fileExists <- doesFileExist fp
  unless fileExists $ ioError $ userError ("File does not exist: " ++ fp)
  C.withImageSurfaceFromPNG fp $ \srf -> do
           C.imageSurfaceGetWidth srf >>= writeIORef S.maxWidth
           C.imageSurfaceGetHeight srf >>= writeIORef S.maxHeight
           unpackSurface srf >>= f
                 
-- | Rasterize the drawing and save it to a file
drawingToFile :: DnaDrawing -> Int -> IO ()
drawingToFile d n = do
  [mw, mh] <- mapM readIORef [S.maxWidth, S.maxHeight]
  C.withImageSurface C.FormatRGB24 mw mh $ \surface -> do
                      C.renderWith surface $ render d
                      dirExists <- doesDirectoryExist subdir
                      unless dirExists $ createDirectory subdir
                      C.surfaceWriteToPNG surface filePath
    where filePath = subdir </> show n <.> "png"
          subdir = "images"
