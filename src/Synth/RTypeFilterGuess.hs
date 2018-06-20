module Synth.RTypeFilterGuess where

import Types.Common
import Types.Filter

import Analysis.FFT

import Utils

guessInitFilter :: AudioFormat -> AudioFormat -> Thetas
guessInitFilter in_audio out_audio = let
  peaks1 = peakList in_audio
  peaks2 = peakList out_audio
  -- the highest threshold for a lpf is the strongest refinement type we can guess
  lpf_init = takeLast fst (map (lpf_refinement_template peaks1 peaks2) lpf_thresholds)
 in 
  initFilter {_lpfThreshold = maybe 0 (invFreqScale . snd) lpf_init } --TODO replace 0
 
lpf_thresholds = [300,400..15000]


-- TODO maybe returns a probabilty/score?
-- | returns wheather the refinement is true on two peakLists, and the threshold used

-- Peak = (freq, amp, phase)

-- | have the amplitudes of the freqs greater than the threshold decreased?
--   the amps dont need to match, we are just checking all freqs above thres
lpf_refinement_template :: [Peak] -> [Peak] -> Double -> (Bool,Double)
lpf_refinement_template ps1 ps2 thres = let
  --freqAmpDecrease (f1,a1,_) (f2,a2,_) = 
  thresFreqs = filter (\(f,_,_) -> fromIntegral f > thres) 
  ps1' = thresFreqs ps1
  ps2' = thresFreqs ps2
  in (True, thres) 

--hpf_refinements :: [([Peak] -> [Peak] -> Bool)]

