module Main where

import Synth.Synth
import Settings
import Analysis.FilterDistance
import Types.Common
import Types.Filter

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
  synthCode settings
