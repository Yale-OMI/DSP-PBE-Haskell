module Main where

import Analysis.FFT
import Data.Audio
import Codec.Wav
import Data.Int

import Types.Common

import System.Exit

import Control.Concurrent.ParallelIO.Global

import PrintAudio
----------
--
--  Tests the aural distance function for sanity
--
----------

dir = "Sounds/AnalysisBenchmarks/"
main :: IO ()
main' = printAudio $ dir++"PianoC.wav"


filenames = 
  [ "PianoC.wav"
  , "PianoFilter.wav"
  , "HornCSharp.wav"
  , "PianoCSharp.wav"]


main = do
  ws <- parallel $ map getFile filenames
  let tuplify [w1,w2,w3,w4] = (w1,w2,w3,w4)
  let (w1,w2,w3,w4) = tuplify ws
  let t1v1 = peakResults w1 w1
  let t1v2 = peakResults w1 w2
  let t1v3 = peakResults w1 w3
  let t3v1 = peakResults w3 w1
  let t1v4 = peakResults w1 w4
  let t4v1 = peakResults w4 w1

  let t3v4 = peakResults w3 w4

  mapM_ print $ zip [1..] filenames
  --identity
  checkTest "id d(1,1)" 0 (==) t1v1

  --assocativty
  checkTest "assoc d(1,3)=d(3,1)" (abs $ t1v3-t3v1) (<) 3
  checkTest "assoc d(1,4)=d(4,1)" (abs $ t1v4-t4v1) (<) 3

  --filter is less than pitch
  checkTest "filter d(1,2) less than pitch d(1,4)" t1v2 (<) t1v4

  --filter is less than instrument
  checkTest "filter d(1,2) less than instrument d(1,3)" t1v2 (<) t1v3
  
  --instrument is more than 500
  checkTest "instrument d(1,3) > 500" t1v3 (>) 500

  --pitch is more than 500
  checkTest "pitch d(1,4) > 500" t1v4 (>) 500

  

  stopGlobalPool

getFile :: FilePath -> IO(AudioFormat)
getFile filepath = do
  let f x = either (error "failed") id x
  w <- importFile $ dir++filepath :: IO(Either String (AudioFormat))
  return $ f w 

checkTest :: String -> Double -> (Double-> Double-> Bool) -> Double -> IO()
checkTest tName v1 f v2 = do
  putStrLn $ "Running: "++tName
  putStrLn $ "  result:    "++(show v1)
  putStrLn $ "  threshold: "++(show v2)
  if v1 `f` v2
    then return ()
    else print "FAILED" >> exitFailure
