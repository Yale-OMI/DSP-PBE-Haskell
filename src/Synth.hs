module Synth where


import FFT
import Data.Audio
import Codec.Wav
import Data.Int

-- | generate the Vivid program to turn the in_example to the out_example
synth :: FilePath -> FilePath -> String
synth in_example out_example = do 
  w1 <- importFile in_example  :: IO(Either String (Audio Int16))
  w2 <- importFile out_example :: IO(Either String (Audio Int16))
  let diff = peakResults w1 w2

trySynth :: _ -> Audio Int16 -> Audio Int16 -> AuralDistance
