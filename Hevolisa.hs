{-# LANGUAGE PatternGuards #-}
--
-- Module      : Hevolisa
-- Copyright   : (c) Daniel Neun 2008
-- License     : BSD-style
-- Maintainer  : daniel.neun@gmx.de
-- Stability   : experimental
-- Portability : portable

module Main where

import Hevolisa.Evolution (start)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(..),exitWith)
import System.IO (hPutStrLn,stderr,stdout)
import System.IO.Error

data Flag = Help | F FilePath
            deriving Eq

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Show this help message"]


main :: IO ()
main = parseArgs >> return ()

parseArgs = do
  argv <- getArgs
  case parse argv of
    (opts,files,[])
        | Help `elem` opts                 -> help
        | not $ null files                 -> tryStart $ head files
    (_,_,errs)                             -> die errs
  where parse argv = getOpt Permute options argv
        header = "Usage: hevolisa PNGFILE"
        info = usageInfo header options
        dump = hPutStrLn stderr
        die errs = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
        help = dump info                      >> exitWith ExitSuccess
        tryStart path = do result <- try (start path)
                           case result of
                             Left e   -> putStrLn $ show e
                             Right _  -> return ()