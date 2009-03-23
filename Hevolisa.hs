{-# LANGUAGE PatternGuards #-}
--
-- Module      : Hevolisa
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Main where

import Hevolisa.Evolution ( evolve, EvolutionOptions(..) )
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(..),exitWith)
import System.IO (hPutStrLn,stderr,stdout)
import System.IO.Error

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

evolutionOptions :: Options -> EvolutionOptions
evolutionOptions opts = EvolutionOptions
                        { eoResize = optResize opts
                        }

main :: IO ()
main = parseArgs >> return ()

parseArgs = do
  argv <- getArgs
  Just (os, fs) <- parseOpts argv
  case (os, fs) of
    (opts, files)
        | optHelp opts        -> help
        | not $ null files    -> tryStart opts $ head files
  where 
    parseOpts argv = case getOpt Permute options argv of
                       (opts, files, []) -> return $ Just (foldl (flip id) defaultOptions opts, files) 
                       (_, _, errs)      -> die errs >> return Nothing
    header = "Usage: hevolisa PNGFILE"
    info = usageInfo header options
    dump = hPutStrLn stderr
    die errs = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
    help = dump info                      >> exitWith ExitSuccess
    tryStart opts path = do 
              result <- try (evolve (evolutionOptions opts) path)
              case result of
                Left e   -> putStrLn $ show e
                Right _  -> return ()