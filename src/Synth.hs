{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Synth where

import Codec.Wav

import SGD

import FFT
import VividRunner
import Vivid

import Types.Common
import Types.Filter

-- | generate the Vivid program to turn the in_example to the out_example
synthCode :: (FilePath, AudioFormat) -> (FilePath, AudioFormat) -> IO (Filter)
synthCode (in_filepath,in_audio) (out_filepath,out_audio) = do
  let initFilter = LPF 800
  synthedFilter <- refineFilter in_filepath out_audio initFilter
  return synthedFilter

{-optimize = 
  multiVarSGD 
    [lpfThreshold, hpfThreshold]
    2 0.1 0.001 
    (Thetas {_lpfThreshold=500,_hpfThreshold=500}) (Thetas {_lpfThreshold=0,_hpfThreshold=0}) evalDriver
-}

-- | Adjust the params of a filter to get the best score
refineFilter :: FilePath -> AudioFormat -> Filter -> IO Filter
refineFilter i o initF = do 
  let tester f = testFilter i o $ toVivid f
  let newF = increaseFilter f 
  newScore <- testFilter i o $ toVivid newF
  if newScore<score
  then do
         print newF
         refineFilter i o newF
  else return f
 
increaseFilter :: Filter -> Filter
increaseFilter = \case 
    HPF t -> HPF $ t+(t*step)
    LPF t -> LPF $ t+(t*step)
    Compose f f' -> Compose (increaseFilter f) (increaseFilter f') --TODO this could be problemati
  where
    step = 0.2

testFilter :: FilePath -> AudioFormat -> (SDBody' '[] Signal -> SDBody' '[] Signal) -> IO AuralDistance
testFilter in_fp outAudio vividCode = do
  newOutFilepath <- runFilter in_fp vividCode
  newAudio <- importFile newOutFilepath :: IO(Either String AudioFormat)
  case newAudio of
    Left e -> error e
    Right a -> return $ peakResults outAudio a

