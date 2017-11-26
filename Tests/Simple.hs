module Main where

import FFT
import Data.Audio
import Codec.Wav
import Data.Int


dir = "Sounds/"
main :: IO ()
main = do
  w1 <- importFile $ dir++"PianoC.wav" :: IO(Either String (Audio Int16))
  w2 <- importFile $ dir++"PianoSoft.wav" :: IO(Either String (Audio Int16))
  w3 <- importFile $ dir++"PianoFilter.wav" :: IO(Either String (Audio Int16))
  w4 <- importFile $ dir++"HornC.wav" :: IO(Either String (Audio Int16))
  w5 <- importFile $ dir++"PianoCSharp.wav" :: IO(Either String (Audio Int16))
  let test1v2 = peakResults w1 w2
  let test1v3 = peakResults w1 w3
  let test1v4 = peakResults w1 w4
  let test1v5 = peakResults w1 w5
  print test1v2
  print test1v3
  print test1v4
  print test1v5
