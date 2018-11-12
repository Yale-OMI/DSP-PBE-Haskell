-- TODO Move to Analysis
module Synth.RTypeFilterGuess where

import Types.Common
import Types.Filter
import Types.Thetas

import Analysis.FFT

import Text.Printf
import Utils

import Data.List 
import Data.Maybe

-- | The first filter we start with is a full parallel composition
guessInitFilter :: (FilePath,AudioFormat) -> (FilePath,AudioFormat) -> IO Filter
guessInitFilter in_audio out_audio = do

  peaks1 <- peakList in_audio
  peaks2 <- peakList out_audio

  let lpf_init = lpf_refinement peaks1 peaks2
  let hpf_init = hpf_refinement peaks1 peaks2
  -- if we aren't applying lpf or hpf, we need to pipe the input back out
  let idapp = if isNothing lpf_init && isNothing hpf_init then 1 else (-1)

  return $
    AmpApp 1 $
      Compose (LPF (fromMaybe 0 lpf_init) (maybe (-1) (\x -> 1) lpf_init)) $
        Compose (HPF (fromMaybe 0 hpf_init) (maybe (-1) (\x -> 1) hpf_init)) $
          Compose (PitchShift 0 (-1)) $
            Compose (Ringz 0 0 (-1)) $
              Compose (ID idapp) $
                (WhiteNoise (-1))

 
-- | As a very rough estimate, if the max freq peak of the output is less than the max freq peak of input
--   we need a lpf, and it should have a value a bit less than the max peak of output
lpf_refinement :: [[Peak]] -> [[Peak]] -> Maybe Float
lpf_refinement ps1 ps2 = let
  getMax = maximum . concat . map (map fst)
 in 
  if getMax ps1 > getMax ps2
  then Just $ invFreqScale $ realToFrac $ (getMax ps2) - 1000
  else Nothing


-- | Use the same idea as lpf to hpf
--   As a very rough estimate, if the min freq peak of the output is greater than the min freq peak of input
--   we need a hpf, and it should have a value a bit more than the max peak of output
hpf_refinement :: [[Peak]] -> [[Peak]] -> Maybe Float
hpf_refinement ps1 ps2 = let
  getMin = minimum . concat . map (map fst)
 in 
  if getMin ps1 < getMin ps2
  then Just $ invFreqScale $ realToFrac $ (getMin ps2) + 1000
  else Nothing




