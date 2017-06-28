module Untitled where

import FFT
import Codec.Wav
import Data.Audio
import Data.Int
import Data.Array
import Data.Complex
import Numeric.Transform.Fourier.FFT
import qualified Settings as S
main :: IO()
main = do
  w1 <- importFile "PianoC.wav" :: IO(Either String (Audio Int16))
  w2 <- importFile "PianoCSharp.wav" :: IO(Either String (Audio Int16))
  w3 <- importFile "HornC.wav" :: IO(Either String (Audio Int16))
  let wav = wavList w1
  let frame = head $ mkFrames wav
  let aframe = listArray (0,(S.frameRes - 1)) frame
  let points = constellate $ rfft aframe
  --print $ tail $ take 2048 $ assocs points
  print points
