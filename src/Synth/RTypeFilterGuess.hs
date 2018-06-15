module Synth.RTypeFilterGuess where

import Types.Common
import Types.Filter

import Analysis.FFT


guessInitFilter :: AudioFormat -> AudioFormat -> Thetas
guessInitFilter in_audio out_audio = let
  peaks1 = peakList in_audio
  peaks2 = peakList out_audio
  lpf_init = undefined --search lpf_refinements for the right one
 in 
  initFilter --set lpfThres as lpf_init 
  


-- TODO maybe returns a probabilty/score?
lpf_refinements :: [([Peak] -> [Peak] -> Bool)]
lpf_refinements = 
  map lpf_base_ref [300,400..5000]

-- Peak = (freq, amp, phase)

-- | the amplitudes of the freqs greater than the threshold have decreased
lpf_base_ref :: Int -> [Peak] -> [Peak] -> Bool
lpf_base_ref thres ps1 ps2 = let
  --filter
  in True 

--hpf_refinements :: [([Peak] -> [Peak] -> Bool)]

