{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Synth where

import Codec.Wav
import System.Random

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
  runFilter "tmp2/final.wav" in_filepath $ toVivid synthedFilter
  return synthedFilter

--thetaSelectors = [lpfThreshold, hpfThreshold,ringzFreq,ringzDecaySecs,ringzApp,lpfApp,hpfApp,whiteApp,ampApp]
thetaSelectors = [lpfThreshold, hpfThreshold,lpfApp,hpfApp,whiteApp,ampApp]

optimize rGen tester = multiVarSGD 
    thetaSelectors
    rGen
    5 --batch size (how many directions to test)
    0.01 --convergance goal
    0.001 --learn rate
    (Thetas {_lpfThreshold=(-0.8),_hpfThreshold=(-1),_ringzFreq=1,_ringzDecaySecs=1,_ringzApp=(-1),_lpfApp=(1),_hpfApp=(-1),_whiteApp=(-1),_ampApp=1})--start apps at 0.5 so they can go up or down
    (Thetas {_lpfThreshold=2,_hpfThreshold=2,_ringzFreq=2,_ringzDecaySecs=2,_ringzApp=2,_lpfApp=2,_hpfApp=2,_whiteApp=1,_ampApp=1}) 
    H.empty 
    tester

-- | Adjust the params of a filter to get the best score
refineFilter :: FilePath -> AudioFormat -> Filter -> IO Filter
refineFilter i o initF = do 
  let tester = testFilter i o . thetaToFilter
  rGen <- getStdGen 
  solution <- optimize rGen tester
  return $ thetaToFilter $ fst solution
 

testFilter :: FilePath -> AudioFormat -> Filter -> IO AuralDistance
testFilter in_fp outAudio f= do
  let vividCode = toVivid f
  print f
  newOutFilepath <- runFilter "tmp/out.wav" in_fp vividCode
  newAudio <- importFile newOutFilepath :: IO(Either String AudioFormat)
  case newAudio of
    Left e -> error e
    Right a -> return $ peakResults outAudio a

