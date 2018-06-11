 {-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Synth.Synth where

import System.Random

import Synth.SGD

import Analysis.FilterDistance

import Types.Common
import Types.Filter

import qualified Data.HashMap.Strict as H



-- | initial points for thetas in GD
--   this can have a large impact on the effectivness of learning
--   might consider randomize restart as well
--   TODO: init value depend on the refinement type inference results
initFilter = (Thetas {
  _lpfThreshold=(-0.8), _lpfApp=(1),
  _hpfThreshold=(-1), _hpfApp=(-1),
  _ringzFreq=1, _ringzDecaySecs=1, _ringzApp=(-1),
  _whiteApp=(-1),
  _ampApp=1})

-- | generate the Vivid program to turn the in_example to the out_example
synthCode :: (FilePath, AudioFormat) -> (FilePath, AudioFormat) -> IO (Filter)
synthCode (in_filepath,in_audio) (out_filepath,out_audio) = do
  synthedFilter <- refineFilter in_filepath out_audio initFilter
  runFilter "tmp2/final.wav" in_filepath $ toVivid synthedFilter
  return synthedFilter

-- | selects the thetas should we vary during GD
--thetaSelectors = [lpfThreshold, hpfThreshold,ringzFreq,ringzDecaySecs,ringzApp,lpfApp,hpfApp,whiteApp,ampApp]
--thetaSelectors = [lpfThreshold, lpfApp, hpfThreshold, hpfApp, whiteApp, ampApp]
thetaSelectors = [lpfThreshold, lpfApp, ampApp]

optimize rGen tester initFilter = multiVarSGD
    thetaSelectors
    rGen
    4 --batch size (how many directions to test)
    0.01 --convergance goal
    0.001 --learn rate
    initFilter
    (Thetas {_lpfThreshold=2,_hpfThreshold=2,_ringzFreq=2,_ringzDecaySecs=2,_ringzApp=2,_lpfApp=2,_hpfApp=2,_whiteApp=1,_ampApp=1})
    H.empty
    tester

-- | Adjust the params of a filter to get the best score
refineFilter :: FilePath -> AudioFormat -> Thetas -> IO Filter
refineFilter i o initF = do
  let tester = testFilter i o . thetaToFilter
  rGen <- getStdGen
  solution <- optimize rGen tester initF
  return $ thetaToFilter $ fst solution

