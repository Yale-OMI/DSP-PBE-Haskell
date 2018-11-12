module Main where

import Synth.Synth
import Settings

import Control.Monad
import System.Console.CmdArgs

main = do
  synthCode defaultOptions
              { inputExample = "Sounds/SynthesisBenchmarks/Constructed/BTS-DNA.wav" 
              , outputExample = "Sounds/SynthesisBenchmarks/Constructed/BTS-DNA-lpf2000.wav"
              }
