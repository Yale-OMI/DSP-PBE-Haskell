module Working where

import FFT
import Codec.Wav
import Data.Audio
import Data.Int
import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT
import Data.Ord
import Data.List
import qualified Settings as S
main :: IO()
main = do
  w1 <- importFile "PianoC.wav" :: IO(Either String (Audio Int16))
  w2 <- importFile "PianoSoft.wav" :: IO(Either String (Audio Int16))
  w3 <- importFile "PianoFilter.wav" :: IO(Either String (Audio Int16))
  w4 <- importFile "PianoCSharp.wav" :: IO(Either String (Audio Int16))
  w5 <- importFile "HornC.wav" :: IO(Either String (Audio Int16))
  print $ peakResults w1 w2
  print $ peakResults w1 w3
  print $ peakResults w1 w4
  print $ peakResults w4 w5
  print $ peakResults w1 w5
  return()
