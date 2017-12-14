module Main where

import FFT
import Data.Audio
import Codec.Wav
import Data.Int

import System.Exit

dir = "Sounds/"
main :: IO ()
main = do
  w1 <- importFile $ dir++"PianoC.wav" :: IO(Either String (Audio Int16))
  w2 <- importFile $ dir++"PianoSoft.wav" :: IO(Either String (Audio Int16))
  w3 <- importFile $ dir++"PianoFilter.wav" :: IO(Either String (Audio Int16))
  w4 <- importFile $ dir++"HornC.wav" :: IO(Either String (Audio Int16))
  w5 <- importFile $ dir++"PianoCSharp.wav" :: IO(Either String (Audio Int16))
  let t1v1 = peakResults w1 w1
  let t1v3 = peakResults w1 w3
  let t1v4 = peakResults w1 w4
  let t4v1 = peakResults w4 w1
  let t1v5 = peakResults w1 w5
  let t5v1 = peakResults w5 w1

  --identity
  checkTest 0 (==) t1v1

  --assocativty
  checkTest (abs $ t1v4-t4v1) (<) 3
  checkTest (abs $ t1v5-t5v1) (<) 3

  --filter is less than pitch
  checkTest t1v3 (<) t1v5

  --filter is more than instrument
  checkTest t1v5 (<) t1v4

checkTest :: Double -> (Double-> Double-> Bool) -> Double -> IO()
checkTest v1 f v2 = do
  print ("Running: "++(show v1)++" "++(show v2))
  if v1 `f` v2
    then return ()
    else print "FAILED" >> exitFailure
