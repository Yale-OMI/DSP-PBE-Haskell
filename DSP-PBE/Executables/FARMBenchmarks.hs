module Main where

import Synth.Synth
import Settings

import Control.Monad
import System.Console.CmdArgs
import System.CPUTime


results_file = "farm_benchmark_results.txt"
main = do
  
  writeFile results_file "Input, Output, Solution, Score, Time (sec)"

  runBenchmark $ 
    defaultOptions
        { inputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav" 
        , outputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010-hpf1500.wav"
        , epsilon = 10 }

  runBenchmark $ 
    defaultOptions
        { inputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav" 
        , outputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010-lpf800.wav"
        , epsilon = 10 }

  runBenchmark $ 
    defaultOptions
        { inputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav" 
        , outputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010-lpf5000.wav"
        , epsilon = 10 }

  runBenchmark $ 
    defaultOptions
        { inputExample = "Sounds/SynthesisBenchmarks/Constructed/BTS-DNA.wav" 
        , outputExample = "Sounds/SynthesisBenchmarks/Constructed/BTS-DNA-lpf2000.wav"
        , epsilon = 10 }

  runBenchmark $ 
    defaultOptions
        { inputExample = "Sounds/SynthesisBenchmarks/Constructed/Holst.wav" 
        , outputExample = "Sounds/SynthesisBenchmarks/Constructed/Holst-hpf3500.wav"
        , epsilon = 50 } --this one seems to be more difficult, so lets set a higher epsilon


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
