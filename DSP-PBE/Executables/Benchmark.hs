module Main where

import Synth.Synth
import Settings

import Control.Monad
import System.Console.CmdArgs

main = do
  synthCode defaultOptions
              { inputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav" 
              , outputExample = "Sounds/SynthesisBenchmarks/Constructed/cartoon010-hpf1500.wav"
              }
