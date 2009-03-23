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
writeInterval = 1000

data Flag = Help 
          | Resize String
            deriving Eq

data Options = Options
    { optHelp :: Bool
    , optResize :: Float
    } deriving ( Show )

defaultOptions = Options
                 { optHelp = False
                 , optResize = 1.0
                 }

options :: [OptDescr (Options -> Options)]
options = [ Option ['h'] ["help"] (NoArg (\opts -> opts { optHelp = True } ))
                   "Show this help message"
          , Option [] ["resize"] (ReqArg (\r opts -> opts { optResize = read r } ) "ratio")
                   "Resize the output images to <ratio> times the original"
          ]

main :: IO ()
main = do
  argv <- getArgs
  Just (os, fs) <- parseOpts argv
  case (os, fs) of
    (opts, files)
        | optHelp opts        -> help
        | not $ null files    -> start opts (head files) `catch` somethingErred "Main"
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

start opts path = do
  e <- startEvolution
  let loop i = do
        g <- readChan (echan e)
        printf "Generation %d: d = %d\n" i (delta g)
        maybeWriteToFile i g
        loop $ i+1
  loop 0
      where 
        maybeWriteToFile n ec
            | isTimeToWrite n = drawingToFile (drawing ec) (width ec) (height ec) n
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
                                us <- unpackSurface srf
                                forkIO $ evolve gens w h us `catch` somethingErred "Evolve"
                                return (w, h)
                    return $ Evolver gens w h


somethingErred :: String -> SomeException -> IO ()
somethingErred s e = do
  printf "%s process failed with:\n" s
  printf "\t %s\n" (show e)
  exitFailure