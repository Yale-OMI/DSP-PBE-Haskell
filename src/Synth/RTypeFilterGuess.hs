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
  -- TODO rethink this, since the types of peaks1 and peaks2 have now changed to [[Peak]]
  lpf_init = takeLast fst (map (lpf_refinement_template peaks1 peaks2) lpf_thresholds)
 in 
  initFilter {_lpfThreshold = maybe 0 (invFreqScale . snd) $ trace ((listToCSV $ head peaks1) ++ "\n" ++ (listToCSV $ head peaks2)) lpf_init } --TODO replace 0

 
lpf_thresholds = [350,400..1500]


-- TODO maybe returns a probabilty/score?
-- | returns wheather the refinement is true on two peakLists, and the threshold used

-- Peak = (freq, amp, phase)

-- | We can find a ref type for lpf by taking the integral of the amplitudes of freq up to a threshold
--   As soon as we detect a decrease in the amps, it is possible we have a lpf at that threshold
lpf_refinement_template :: [[Peak]] -> [[Peak]] -> Double -> (Bool,Double)
lpf_refinement_template ps1 ps2 thres = let
  thresFreqs = map (filter (\(f,_,_) -> fromIntegral f < thres))
  sumAmps = sum . map getAmp
  ps1Integral = sum $ map sumAmps $ thresFreqs ps1
  ps2Integral = sum $ map sumAmps $ thresFreqs ps2
 in 
  (trace ((show thres) ++ " - " ++ (show (ps1Integral - ps2Integral)) ++ "\n")
          -- ++ (show $ head ps1) ++ " \n ---- \n " ++ (show $ head ps2))
    ps1Integral > ps2Integral, thres)

--hpf_refinements :: [([Peak] -> [Peak] -> Bool)]




