{-# LANGUAGE PartialTypeSignatures #-}

module Synth where

import Codec.Wav

import FFT
import VividRunner

import Types.Common

-- | generate the Vivid program to turn the in_example to the out_example
synthCode :: (FilePath, AudioFormat) -> (FilePath, AudioFormat) -> IO String
synthCode (in_filepath,in_audio) (out_filepath,out_audio) = do 
  testFilter in_filepath out_audio lpFilter >>= print
  return "generated code"

testFilter :: FilePath -> AudioFormat -> _ -> IO AuralDistance
testFilter in_fp outAudio vividCode = do
  newOutFilepath <- runFilter in_fp vividCode
  newAudio <- importFile newOutFilepath :: IO(Either String AudioFormat)
  case newAudio of
    Left e -> error e
    Right a -> return $ peakResults outAudio a
