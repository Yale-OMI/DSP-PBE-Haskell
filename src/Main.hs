module Main where

import FFT
import Codec.Wav
import Data.Audio
import Data.Either
import Data.Int

main :: IO()
main = do
  w1 <- importFile "Sounds/PianoC.wav" :: IO(Either String (Audio Int16))
  w2 <- importFile "Sounds/PianoSoft.wav" :: IO(Either String (Audio Int16))
  w3 <- importFile "Sounds/PianoFilter.wav" :: IO(Either String (Audio Int16))
  w4 <- importFile "Sounds/PianoCSharp.wav" :: IO(Either String (Audio Int16))
  w5 <- importFile "Sounds/HornC.wav" :: IO(Either String (Audio Int16))
  let test1v2 = peakResults w1 w2
  let test1v3 = peakResults w1 w3
  let test1v4 = peakResults w1 w4
  let test1v5 = peakResults w1 w5
  let test4v5 = peakResults w4 w5
  putStrLn $ "Volume-reduced is less than filtered: " ++ (show $ test1v2 < test1v3)
  putStrLn $ "Filtered is less than different note: " ++ (show $ test1v3 < test1v4)
  putStrLn $ "Different note is less than different note and different instrument: " ++ (show $ test1v4 < test1v5)
  return()
