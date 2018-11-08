module Main where

import System.Directory
import System.Environment
import GHC.IO.Encoding
import System.FilePath

import Control.Monad

import Synth

main :: IO()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  args <- getArgs
  when (length args /= 2)
    $ error "Usage: synth <example input file> <example output file>"
  
  in_exists <- doesFileExist $ head args
  out_exists <- doesFileExist $ head $ tail args
  when (not in_exists || not out_exists)
    $ error ("File not found: " ++ 
          (if not in_exists then (head args) else (head $ tail args)))
  in_example <- readFile $ head args
  out_example <- readFile $ head $ tail args
  print $ synthCode in_example out_example


