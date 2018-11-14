module Utils where

import Synth.Synth
import Settings

import System.CPUTime


writeHeader results_file = do
  writeFile results_file "Input, Output, Solution, Score, Structural Attempts, Time (sec)"

runBenchmark results_file options = do
  start <- getCPUTime
  (solution, score, structureAttempts) <- synthCode options
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  appendFile results_file $ "\n" ++
    (inputExample options)++", "++
    (outputExample options)++", "++
    (show solution)++", "++
    (show score )++", "++
    (show structureAttempts )++", "++
    (show diff)
