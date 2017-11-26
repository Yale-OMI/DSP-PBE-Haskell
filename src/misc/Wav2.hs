module Wav2 where

import Data.Audio
import Codec.Wav
import Data.Int
import Data.Either

main :: IO ()
main = do
  w1 <- importFile "Test.wav" :: IO(Either String (Audio Int16))
  w2 <- importFile "LPFilterTest.wav" :: IO(Either String (Audio Int16))
  print $ sampleData $ head $ rights[w2]
  print ((sampleData $ head $ rights[w1]) == (sampleData $ head $ rights[w2]))
  return ()
