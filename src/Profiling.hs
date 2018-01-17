module Main where

import Synth
import Types.Common

import System.Directory
import System.Environment
import GHC.IO.Encoding
import System.FilePath
import Codec.Wav

import Control.Monad

main = do
  let inEx = "Sounds/A440.wav"
      outEx = "Sounds/A440-lpf1000.wav"
  fileActions <- mapM importFile [inEx,outEx] :: IO [Either String (AudioFormat)]
  case sequence fileActions of
    Right fs -> synthCode (inEx, head fs) (outEx, head $ tail fs) >>= print
    Left e -> error e
  return ()


