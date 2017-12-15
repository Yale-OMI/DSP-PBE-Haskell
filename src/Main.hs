module Main where

import System.Directory
import System.Environment
import GHC.IO.Encoding
import System.FilePath
import Codec.Wav

import Control.Monad

import Synth
import Types.Common

main :: IO()
main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8
  args <- getArgs
  when (length args /= 2)
    $ error "Usage: synth <example input file> <example output file>"
 
  fileActions <- mapM importFile args :: IO [Either String (AudioFormat)]
  case sequence fileActions of
    Right fs -> synthCode (head args, head fs) (head $ tail args, head $ tail fs) >>= print
    Left e -> error e
  return ()


