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

data Flag = Help | F FilePath
            deriving Eq

options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help)
                   "Show this help message",
            Option ['f'] ["file"] (ReqArg (\s -> F s) "FILE")
                   "Path to source image file" ]


main :: IO ()
main = parseArgs >> return ()

parseArgs = do
  argv <- getArgs
  case parse argv of
    (opts,files,[])
        | Help `elem` opts                 -> help
        | [F path] <- filter (/=Help) opts -> start path
    (_,_,errs)                             -> die errs
  where parse argv = getOpt Permute options argv
        header = "Usage: hevolisa -f file"
        info = usageInfo header options
        dump = hPutStrLn stderr
        die errs = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
        help = dump info                      >> exitWith ExitSuccess