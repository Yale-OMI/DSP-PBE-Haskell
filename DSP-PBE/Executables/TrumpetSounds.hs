module Main where

import Synth.Synth
import Analysis.FFT
import Settings

import Control.Monad
import System.Console.CmdArgs

import Utils
import System.Directory
import System.Timeout


import Types.Common
import Types.Filter
import Codec.Wav

import Data.List

results_file = "trumpet_sounds_results.txt"
main = do
  
  writeHeader results_file

  let
    dir = "Sounds/SynthesisBenchmarks/Recordings/TrumpetSounds/" 
    input = dir++"mute_00_none.wav"
    trumpetConfig = defaultOptions
                      { inputExample = input }

  allMuteSounds <- listDirectory dir

  let 
    allMuteSounds' = map (dir++) $ sort allMuteSounds

    oneSecond = 1000000
    runOne fp =
      runBenchmarkTimed (7 * 60 * oneSecond) results_file $ 
        trumpetConfig {
              outputExample = dir++fp
            , smartStructuralRefinement = True
            , thetaLogSizeTimeout = 4
            , filterLogSizeTimeout = 5
            , epsilon = 10 } 
      

--  allMuteAudio <- mapM getFile allMuteSounds'
--  let as = zip allMuteSounds' allMuteAudio

--  mapM (\a -> auralDistance (head as) a >>= (\x -> print ((fst a)++"    "++show x)) ) as


--  mapM_ runOne  allMuteSounds
  runOne "mute_01_hat.wav"



getFile :: FilePath -> IO(AudioFormat)
getFile filepath = do
  let f x = either (error "failed") id x
  w <- importFile $ filepath :: IO(Either String (AudioFormat))
  return $ f w 

