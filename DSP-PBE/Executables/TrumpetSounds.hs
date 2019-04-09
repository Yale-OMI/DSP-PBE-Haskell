module Main where

import Synth.Synth
import Settings

import Control.Monad
import System.Console.CmdArgs

import Utils
import System.Directory
import System.Timeout

results_file = "trumpet_sounds_results.txt"
main = do
  
  writeHeader results_file

  let
    dir = "Sounds/SynthesisBenchmarks/Recordings/TrumpetSounds/" 
    input = dir++"mute_00_none.wav"
    trumpetConfig = defaultOptions
                      { inputExample = input }

    oneSecond = 1000000
    runOne fp =
      runBenchmarkTimed (10 * 60 * oneSecond) results_file $ 
        trumpetConfig {
              outputExample = dir++fp
            , smartStructuralRefinement = True
            , thetaLogSizeTimeout = 2
            , filterLogSizeTimeout = 5
            , epsilon = 10 } 
      
  allMuteSounds <- listDirectory dir

  mapM_ runOne allMuteSounds


    
