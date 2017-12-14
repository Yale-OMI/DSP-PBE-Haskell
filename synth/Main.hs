module Main where

import SynthSynth 

main :: IO()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  args <- getArgs
  if length args /= 2 then
    error "Usage: synth <example input file> <example output file>"
  else do
    in_exists <- doesFileExist $ head args
    out_exists <- doesFileExist $ head $ tail args
    if (not in_exists || not out_exists) then
      error ("File not found: " ++ if not in_exists then (head args) else (head $ tail args))
    else do
      in_example <- readFile $ head args
      out_example <- readFile $ head $ tail args
      print $ synthCode in_example out_example


