module Main where

import Synth.Synth
import Settings

import Control.Monad
import System.Console.CmdArgs

import Utils

results_file = "pldi_benchmark_results.txt"
main = do
  
  writeHeader results_file

  runBenchmark results_file $ 
    defaultOptions
        { inputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav" 
        , outputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010-sc-lpf2000.wav"
        , smartStructuralRefinement = False
        , epsilon = 10 
        , learnRate = 0.001
        , converganceGoal = 0.001
        }

{-
  runBenchmark results_file $ 
    defaultOptions
        { inputExample = "Sounds/SynthesisBenchmarks/Constructed/Holst.wav" 
        , outputExample = "Sounds/SynthesisBenchmarks/Constructed/Holst-hpf3500.wav"
        , smartStructuralRefinement = False
        , epsilon = 10 } --using a lower espsilon than in FARM benchmarks, which should trigger the strucutral loop

-}

