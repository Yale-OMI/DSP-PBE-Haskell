-- TODO Move to Analysis
module Synth.RTypeFilterGuess where

import Types.Common
import Types.Filter
import Types.Thetas

import Analysis.FFT

import Text.Printf
import Utils

guessInitFilter :: (FilePath,AudioFormat) -> (FilePath,AudioFormat) -> IO Thetas
guessInitFilter in_audio out_audio = do
  peaks1 <- peakList in_audio
  peaks2 <- peakList out_audio
  let 
    -- TODO replace with a fold
    -- the highest threshold for a lpf is the strongest refinement type we can guess
    lpf_init = takeLast fst (map (lpf_refinement_template peaks1 peaks2) lpf_thresholds)
  
  return $ initFilter {
             -- TODO replace 0
             _lpfThreshold = maybe 0 (invFreqScale . snd) lpf_init } 

 
lpf_thresholds = [350,400..6000]


-- TODO maybe returns a probabilty/score?
-- | returns wheather the refinement is true on two peakLists, and the threshold used

-- Peak = (freq, amp, phase)


-- this is problematic, since the score (As it is wrtten now) will always increase as we add freqs

-- | We can find a ref type for lpf by taking the integral of the amplitudes of freq up to a threshold
--   As soon as we detect a decrease in the amps, it is possible we have a lpf at that threshold
lpf_refinement_template :: [[Peak]] -> [[Peak]] -> Double -> (Bool,Double)
lpf_refinement_template ps1 ps2 thres = let
  thresFreqs = map (filter (\(f,_) -> fromIntegral f < thres))
  sumAmps = sum . map getAmp
  ps1Integral = sum $ map sumAmps $ thresFreqs ps1
  ps2Integral = sum $ map sumAmps $ thresFreqs ps2
 in 
  (trace (
    (show thres) ++ ", " ++ (printf "%.6f" (ps1Integral - ps2Integral))
    ++ ", " ++ (printf "%.6f" (ps1Integral / ps2Integral))
    ++ ", " ++ (show $ length $ concat $ thresFreqs ps1)
    ++ ", " ++ (show $ length $ concat $ thresFreqs ps2))
          -- ++ (show $ head ps1) ++ " \n ---- \n " ++ (show $ head ps2))
    ps1Integral > ps2Integral, thres)

--hpf_refinements :: [([Peak] -> [Peak] -> Bool)]




