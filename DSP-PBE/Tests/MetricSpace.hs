module Main where

import Analysis.FFT
import Data.Audio
import Codec.Wav
import Data.Int

import Types.Common
import Utils 

import TestUtils 

import System.Exit

import Control.Concurrent.ParallelIO.Global

import PrintAudio
----------
--
--  Tests the aural distance function for properties of:
--  1) Metric Space
--  2) Aural reasonability 
--
----------

dir = "Sounds/AnalysisBenchmarks/"
filenames = map (dir++)
  [ "PianoC2.wav"
  , "PianoFilter2.wav"
  , "HornCSharp2.wav"
  , "PianoCSharp2.wav"
  ]

main :: IO ()
--main' = printAudio $ dir++"PianoC.wav"
main = do
  constallationTest
  comparisonTests
  return ()

constallationTest = do
  f <- getFile (dir++"PianoC2.wav")
  p <- peakList (dir++"PianoC2.wav",f)
  putStrLn$ "# of time slices taken from PianoC.wav: "
  print $ length p
  if length p > 1 
    then return ()
    else putStrLn "FAILED - too few time slices taken" >> exitFailure
  putStrLn $ "First time slice peaks of PianoC.wav: "
  putStrLn $ listToCSV $ take 10 $ head p
  

comparisonTests = do
  ws' <- parallel $ map getFile filenames
  let ws = zip filenames ws'
  let tuplify [w1,w2,w3,w4] = (w1,w2,w3,w4)
  let (w1,w2,w3,w4) = tuplify ws
  t1v1 <- auralDistance w1 w1
  t1v2 <- auralDistance w1 w2
  t1v3 <- auralDistance w1 w3
  t3v1 <- auralDistance w3 w1
  t1v4 <- auralDistance w1 w4
  t4v1 <- auralDistance w4 w1
  t3v4 <- auralDistance w3 w4

  mapM_ print $ zip [1..] filenames
  
  print "Checking properties of metric space"
  checkTest "id of indiscernibles d(1,1)" 0 (==) t1v1

  checkTest "symmetry d(1,3)=d(3,1)" (abs $ t1v3-t3v1) (<) 3
  checkTest "symmetry d(1,4)=d(4,1)" (abs $ t1v4-t4v1) (<) 3
  
  checkTest "triangle inequality - d(1,4) less than d(1,3) + d(3,4)" t1v3 (<) (t1v3 + t3v4)

  print "Checking properties specific to aural perception"

  checkTest "filter d(1,2) less than pitch d(1,4)" t1v2 (<) t1v4

  checkTest "filter d(1,2) less than instrument d(1,3)" t1v2 (<) t1v3
  
  checkTest "inst+pitch d(1,3) more than pitch d(1,4)" t1v3 (>) t1v4
  
  
  stopGlobalPool

checkTest :: String -> Double -> (Double-> Double-> Bool) -> Double -> IO()
checkTest tName v1 f v2 = do
  putStrLn $ "Running: "++tName
  putStrLn $ "  result:    "++(show v1)
  putStrLn $ "  threshold: "++(show v2)
  if v1 `f` v2
    then putStrLn "PASSED" >> return ()
    else putStrLn "FAILED" >> exitFailure
