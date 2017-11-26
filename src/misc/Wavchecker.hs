module Wavchecker where

import Data.Audio
import Codec.Wav
import Data.Int
import Data.Either
import Data.Array.IArray
import Numeric.Extra

zeroCross :: [Double] -> Double
zeroCross ds
  |(length ds) == 1                                 = 0
  |((head ds) > 0) && ((head(tail ds)) < 0)          = 1 + (zeroCross (tail ds))
  |((head ds) < 0) && ((head(tail ds)) > 0)          = 1 + (zeroCross (tail ds))
  |otherwise                                        = zeroCross (tail ds)

main :: IO ()
main = do
  w1 <- importFile "Test.wav" :: IO(Either String (Audio Int16))
  w2 <- importFile "Test.wav" :: IO(Either String (Audio Int16))
  let s1 = assocs $ sampleData $ head $ rights[w1]
  let f1 = filter ((\x -> x `mod` 6 ==0).fst) s1
  let d1 = map (toSample.snd) f1
  let s2 = assocs $ sampleData $ head $ rights[w2]
  let f2 = filter ((\x -> x `mod` 6 ==0).fst) s2
  let d2 = map (toSample.snd) f2
  let a1 = (sum d1)/(intToDouble(length d1))
  let a2 = (sum d2)/(intToDouble(length d2))
  print $ d2
  print $ abs((a1 - a2)/a1)
  print $ ((zeroCross d1) - (zeroCross d2))/(zeroCross d1)
  print ((sampleData $ head $ rights[w1]) == (sampleData $ head $ rights[w2]))
  return ()
