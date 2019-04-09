{-# LANGUAGE PartialTypeSignatures #-}

module Utils where

import Synth.Synth
import Settings

import System.CPUTime

import Control.Exception
import Control.Exception.Base
import System.Timeout

writeHeader results_file = do
  writeFile results_file "Input, Output, Solution, Score, Structural Attempts, Time (sec)"

runBenchmark fp opts = 
  runBenchmarkTimed (-1) fp opts


runBenchmarkTimed to fp options = do
  start <- getCPUTime
  result <- try $ timeout to $ synthCode options :: IO (Either SomeException (Maybe _))
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  case result of 
    Left _ ->
      appendFile fp "\nTimed out"
    Right x -> 
      case x of 
            Nothing ->
              appendFile fp "\nTimed out"
            Just (solution, score, structureAttempts) -> 
              appendFile fp $ "\n" ++
                (inputExample options)++", "++
                (outputExample options)++", "++
                (show solution)++", "++
                (show score )++", "++
                (show structureAttempts )++", "++
                (show diff)
