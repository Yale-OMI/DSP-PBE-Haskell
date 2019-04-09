{-# LANGUAGE PartialTypeSignatures #-}

module Utils where

import Synth.Synth
import Settings

import System.CPUTime

import Control.Exception
import Control.Exception.Base
import System.Timeout

import Numeric
import System.FilePath

writeHeader results_file = do
  writeFile results_file "Input, Output, Solution, Score, Structural Attempts, Time (sec)"

runBenchmark fp opts = 
  runBenchmarkTimed (-1) fp opts


runBenchmarkTimed to fp options = do
  start <- getCPUTime
  result <- try $ timeout to $ synthCode options :: IO (Either SomeException (Maybe _))
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  let outputString = formatResult result options diff
  appendFile fp $ outputString


formatResult result options diff =
  "\n" ++
  (takeBaseName $ inputExample options)++", "++
  (takeBaseName $ outputExample options)++", "++
  case result of 
    Left _ -> timeoutText
    Right x -> 
      case x of 
            Nothing -> timeoutText
            Just (solution, score, structureAttempts) -> 
                (show solution)++", "++
                (showFFloat (Just 3) score "")++", "++
                (show structureAttempts )++", "++
                (showFFloat (Just 3) diff "")
 
--TODO should be able to get some info out of a timeout still
timeoutText =
    "None , "
    ++"N/A , " 
    ++"N/A , "
    ++"Timeout"
