--
-- Module      : Hevolisa
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Main where

import Prelude hiding ( catch )

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import qualified Graphics.Rendering.Cairo as C
import Hevolisa.Evolution
import Hevolisa.Renderer
import System.Console.GetOpt
import System.Directory
import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import System.IO ( hPutStrLn, stderr, stdout )
import Text.Printf ( printf )

-- | Number of mutations between image writes
writeInterval = 100

data Flag = Help 
          | Resize String
            deriving Eq

data Options = Options
    { optHelp :: Bool
    , optResize :: Double
    , optShowGen :: Bool
    , optSampleSize :: Double
    } deriving ( Show )

defaultOptions = Options
                 { optHelp = False
                 , optResize = 1.0
                 , optSampleSize = 1.0
                 , optShowGen = False
                 }

options :: [OptDescr (Options -> Options)]
options = [ Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True } ))
                   "Show this help message"
          , Option [] ["resize"] (ReqArg (\r opts -> opts { optResize = read r } ) "ratio")
                   "Resize the output images to <ratio> times the original"
          , Option [] ["show-generation"] (NoArg (\opts -> opts { optShowGen = True} ))
                   "Show the generation in the top-left corner of the images produced"
          , Option [] ["sample-size"] (ReqArg (\r opts -> opts { optSampleSize = read r} ) "ratio")
                   "Scale the image down internally; increases speed but hurts output quality"
          ]

main :: IO ()
main = do
  argv <- getArgs
  Just (os, fs) <- parseOpts argv
  case (os, fs) of
    (opts, files)
        | optHelp opts        -> help
        | not $ null files    -> start opts (head files) `catch` somethingErred "Main"
        | otherwise           -> help
  return ()
    where 
      parseOpts argv = case getOpt Permute options argv of
                         (opts, files, []) -> return $ Just (foldl (flip id) defaultOptions opts, files) 
                         (_, _, errs)      -> die errs >> return Nothing
      header = "Usage: hevolisa PNGFILE"
      info = usageInfo header options
      dump = hPutStrLn stderr
      die errs = dump (concat errs ++ info) >> exitFailure
      help = dump info                      >> exitSuccess

data Evolver = Evolver
    { echan :: Chan EvolutionContext
    , sourceWidth :: Int
    , sourceHeight :: Int
    }

start :: Options -> FilePath -> IO ()
start opts path = do
  e <- startEvolution
  let opts' = opts { optResize = optResize opts * (1.0 / optSampleSize opts) }
  print $ optResize opts'
  let loop i = do
        g <- readChan (echan e)
        printf "Generation %d: d = %d\n" i (delta g)
        if optResize opts' == 1.0
          then maybeWriteToFile i (drawing g) (width g) (height g)
          else do
            let f = optResize opts'
                rd = resizeDrawing f $ drawing g
                rw = round $ (fromIntegral $ width g) * f
                rh = round $ (fromIntegral $ height g) * f
            maybeWriteToFile i rd rw rh
        loop $ i+1
  loop 0
      where 
        maybeWriteToFile n d w h
            | isTimeToWrite n = drawingToFile d w h n ( optShowGen opts )
            | otherwise       = return ()
        isTimeToWrite n = n `mod` writeInterval == 0
        startEvolution = do
                    fileExists <- doesFileExist path
                    unless fileExists $ error $ "File does not exist: " ++ path
                    gens <- newChan
                    printf "Hevolisa evolving %s...\n" path
                    (w, h) <- C.withImageSurfaceFromPNG path $ \srf -> do
                                w <- C.imageSurfaceGetWidth srf
                                h <- C.imageSurfaceGetHeight srf
                                srf' <- C.createImageSurface C.FormatRGB24
                                                             (round $ fromIntegral w * (optSampleSize opts))
                                                             (round $ fromIntegral h * (optSampleSize opts))
                                C.renderWith srf' $ do
                                         C.scale (optSampleSize opts) (optSampleSize opts)
                                         C.setSourceSurface srf 0 0
                                         C.paint
                                us' <- unpackSurface srf'
                                w' <- C.imageSurfaceGetWidth srf'
                                h' <- C.imageSurfaceGetHeight srf'
                                forkIO $ evolve gens w' h' us' `catch` somethingErred "Evolve"
                                return (w', h')
                    return $ Evolver gens w h


somethingErred :: String -> SomeException -> IO ()
somethingErred s e = do
  printf "%s process failed with:\n" s
  printf "\t %s\n" (show e)
  exitFailure