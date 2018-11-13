module Main where

import Synth.Synth
import Settings

import Control.Monad
import System.Console.CmdArgs
import System.CPUTime


results_file = "pldi_benchmark_results.txt"
main = do
  
  writeFile results_file "Input, Output, Solution, Score, Time (sec)"

  runBenchmark $ 
    defaultOptions
        { inputExample = "Sounds/SynthesisBenchmarks/Constructed/Holst.wav" 
        , outputExample = "Sounds/SynthesisBenchmarks/Constructed/Holst-hpf3500.wav"
        , smartStructuralRefinement = False
        , epsilon = 10 } --using a lower espsilon than in FARM benchmarks, which should trigger the strucutral loop


runBenchmark options = do
  start <- getCPUTime
  (solution, score) <- synthCode options
  end   <- getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  appendFile results_file $ "\n" ++
    (inputExample options)++", "++
    (outputExample options)++", "++
    (show solution)++", "++
    (show score )++", "++
    (show diff)
