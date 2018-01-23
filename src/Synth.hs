{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Synth where

import Codec.Wav

import SGD

import FFT
import VividRunner

import Types.Common
import Types.Filter

import qualified Data.HashMap.Strict as H

-- | generate the Vivid program to turn the in_example to the out_example
synthCode :: (FilePath, AudioFormat) -> (FilePath, AudioFormat) -> IO (Filter)
synthCode (in_filepath,in_audio) (out_filepath,out_audio) = do
  let initFilter = undefined
  synthedFilter <- refineFilter in_filepath out_audio initFilter
  return synthedFilter

optimize tester = fst $
  multiVarSGD 
    [lpfThreshold, hpfThreshold]
    2 --batch size (how many directions to test)
    0.0001 --convergance goal
    10 --learn rate
    (Thetas {_lpfThreshold=4000,_hpfThreshold=1,_ringzFreq=30,_ringzApp=0,_lpfApp=0,_hpfApp=0}) 
    (Thetas {_lpfThreshold=1000,_hpfThreshold=1000,_ringzFreq=30,_ringzApp=1,_lpfApp=1,_hpfApp=1}) 
    H.empty 
    tester

-- | Adjust the params of a filter to get the best score
refineFilter :: FilePath -> AudioFormat -> Filter -> IO Filter
refineFilter i o initF = do 
  let tester = testFilter i o . thetaToFilter
  return $ thetaToFilter $ optimize tester
 

testFilter :: FilePath -> AudioFormat -> Filter -> IO AuralDistance
testFilter in_fp outAudio f= do
  let vividCode = toVivid f
  print f
  newOutFilepath <- runFilter in_fp vividCode
  newAudio <- importFile newOutFilepath :: IO(Either String AudioFormat)
  case newAudio of
    Left e -> error e
    Right a -> return $ peakResults outAudio a

