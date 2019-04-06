module Main where

import Synth.Synth
import Settings

import Control.Monad
import System.Console.CmdArgs

import Utils

results_file = "trumpet_sounds_results.txt"
main = do
  
  writeHeader results_file

  let
    dir = "Sounds/SynthesisBenchmarks/Recordings/TrumpetSounds/" 
    input = dir++"mute_00_none.wav"
    trumpetConfig = defaultOptions
                      { inputExample = input }

  runBenchmark results_file $ 
    trumpetConfig {
          outputExample = dir++"mute_06_harmon_joral.wav"
        , smartStructuralRefinement = True
        , thetaLogSizeTimeout = 2
        , filterLogSizeTimeout = 5
        , epsilon = 10 } 


  runBenchmark results_file $ 
    trumpetConfig {
          outputExample = dir++"mute_01_hat.wav"
        , smartStructuralRefinement = True
        , thetaLogSizeTimeout = 2
        , filterLogSizeTimeout = 5
        , epsilon = 10 } 


  runBenchmark results_file $ 
    trumpetConfig {
          outputExample = dir++"mute_02_straight_gold.wav"
        , smartStructuralRefinement = True
        , thetaLogSizeTimeout = 2
        , filterLogSizeTimeout = 5
        , epsilon = 10 } 


  runBenchmark results_file $ 
    trumpetConfig {
          outputExample = dir++"mute_13_plunger.wav"
        , smartStructuralRefinement = True
        , thetaLogSizeTimeout = 2
        , filterLogSizeTimeout = 5
        , epsilon = 10 } 

