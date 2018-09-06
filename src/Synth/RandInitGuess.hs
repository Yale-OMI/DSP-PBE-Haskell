module Synth.RandInitGuess where

import Types.Common
import Types.Filter

import Analysis.FFT
import Analysis.FilterDistance

import Text.Printf
import Utils

import Data.Ord
import Data.List

-- Guess a few filters and test cost to pick best starting point
guessRandInitFilter :: (FilePath,AudioFormat) -> (FilePath,AudioFormat) -> IO Thetas
guessRandInitFilter in_audio out_audio = do
  peaks1 <- peakList in_audio
  peaks2 <- peakList out_audio
  let 
    initFilter' = initFilter { _pitchShiftApp = -1 }
    testFilter' = testFilter (fst in_audio) out_audio
    lows  = map (\thres -> initFilter' { _lpfThreshold = invFreqScale thres, _lpfApp = 1 }) [1000,2000..10000]
    highs = map (\thres -> initFilter' { _hpfThreshold = invFreqScale thres, _hpfApp = 1 }) [1000,2000..10000]
    lowAndHighs = map (\(thres1,thres2) -> initFilter' { 
                                     _lpfThreshold = invFreqScale thres1
                                   , _lpfApp = 1
                                   , _hpfThreshold = invFreqScale thres2
                                   , _hpfApp = 1 } )
                      $ zip [1000,2000..10000] [10000,9000..1000]
    keyInits = lows ++ highs ++ lowAndHighs
  vals <- mapM testFilter' $ map thetaToFilter keyInits
  --print $ zip vals keyInits
  let bestInit = minimumBy (comparing fst) $ zip vals keyInits
  -- TODO provide modular way (settings?) or override the initial filter guess
  return $ (snd bestInit) { _pitchShiftApp = -1, _ringzApp = -1, _whiteApp = -1 , _lpfApp = -1}
