{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Analysis.FFT_Py (peakListPython) where

import Types.Common
import qualified Settings as S

import Shelly (shelly, silently, run)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B 
import Data.String
import Utils

import Data.Csv
import qualified Data.Vector as V
import Data.Either

import Utils

default (T.Text)


-- Use the python script to get sliding window fft results
-- I would rather not to this, but the Haskell one just isnt working
peakListPython :: FilePath -> IO (OverTime (OverFreq Peak))
peakListPython audio_fp = do
  results <- shelly $ silently $ run (fromString "python") 
           [  "PythonUtils/fft.py"
            , T.pack $ audio_fp
            , T.pack $ show S.frameRes
            , T.pack $ show S.overlap]
 
  return $ parseCSV results


-- Expects the python module to return a single string, with '#' for each time slice
parseCSV :: T.Text -> [[(Int, Double)]]
parseCSV resultString = let
  csvList = T.split (=='#') resultString
  decodeTimeSlice csv = decode NoHeader (B.fromStrict $ T.encodeUtf8 csv) :: Either String (V.Vector (Double, Double))
  toPeak = map (\(f,a) -> (floor f, a)) 
  peakList = map (either (\e -> trace e undefined) (toPeak. V.toList) . decodeTimeSlice) $! csvList
 in
  peakList
