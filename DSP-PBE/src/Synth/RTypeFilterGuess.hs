-- TODO Move to Analysis
module Synth.RTypeFilterGuess where

import Types.Common
import Types.Filter
import Types.Thetas

import Analysis.FFT

import Text.Printf
import Utils

import Data.List 

-- | The first filter we start with is a full parallel composition
guessInitFilter :: (FilePath,AudioFormat) -> (FilePath,AudioFormat) -> IO Filter
guessInitFilter in_audio out_audio = do

  peaks1 <- peakList in_audio
  peaks2 <- peakList out_audio

  let lpf_init = takeLast fst (map (lpf_refinement_template peaks1 peaks2) lpf_thresholds)

  return $
    AmpApp 0 $
      Compose (LPF (maybe 0 snd lpf_init) (maybe (-1) (\x -> 1) lpf_init)) $
        Compose (HPF 0 0) $
          Compose (PitchShift 0 (-1)) $
            Compose (Ringz 0 0 (-1)) $
              (WhiteNoise 0)

 
lpf_thresholds = [350] --,400..6000]

-- TODO maybe returns a probabilty/score?
-- | returns wheather the refinement is true on two peakLists, and the threshold used

-- Peak = (freq, amp, phase)

-- this is problematic, since the score (As it is wrtten now) will always increase as we add freqs

-- | We can find a ref type for lpf by taking the integral of the amplitudes of freq up to a threshold
--   As soon as we detect a decrease in the amps, it is possible we have a lpf at that threshold
lpf_refinement_template :: [[Peak]] -> [[Peak]] -> Double -> (Bool,Float)
lpf_refinement_template ps1 ps2 thres = let
  thresFreqs = map (filter (\(f,_) -> fromIntegral f < thres))
  sumAmps = sum . map getAmp
  ps1Integral = sum $ map sumAmps $ thresFreqs ps1
  ps2Integral = sum $ map sumAmps $ thresFreqs ps2
 in 
  (trace (
    (show thres) 
    ++ ", " ++ (show (sort $ concat $ map (map fst) ps1))
    ++ ", " ++ (show (sort $ concat $ map (map fst) ps2))
    ++ ", " ++ (printf "%.6f" (ps1Integral ))
    ++ ", " ++ (printf "%.6f" (ps2Integral ))
    ++ ", " ++ (show $ length $ concat $ thresFreqs ps1)
    ++ ", " ++ (show $ length $ concat $ thresFreqs ps2))
          -- ++ (show $ head ps1) ++ " \n ---- \n " ++ (show $ head ps2))
    ps1Integral > ps2Integral, realToFrac $ invFreqScale thres)

--hpf_refinements :: [([Peak] -> [Peak] -> Bool)]




