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
  let 
      inEx = "Sounds2/cartoon010.wav"
      outEx = "Sounds2/cartoon010-lpf800.wav"
      --inEx = "Sounds2/pinkFloydOrig.wav"
      --outEx = "Sounds2/pinkFloydFilt.wav"
  fileActions <- mapM importFile [inEx,outEx] :: IO [Either String (AudioFormat)]
  case sequence fileActions of
    Right fs -> synthCode (inEx, head fs) (outEx, head $ tail fs) >>= print
    Left e -> error e
  return ()


