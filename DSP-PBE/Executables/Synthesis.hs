module Main where

import Synth.Synth
import Settings
import Analysis.FilterDistance
import Types.Common
import Types.Filter
import Types.SCCode

import System.Directory
import System.Environment
import GHC.IO.Encoding
import System.FilePath
import qualified Codec.Wav as W

import Control.Monad
import System.Console.CmdArgs

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8  

  settings <- cmdArgs defaultOptions
  (soln, score, structs) <- synthCode settings
  putStrLn $ "Found a program with score "++(show score)++" after "++(show structs)++" structural attempts."
  putStrLn $ toSCCode soln
