{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Analysis.FFT_Py (peakListPython) where

import Types.Common

import System.IO.Unsafe

import Shelly (shelly, run)
import qualified Data.Text as T
import Data.String
import Utils

default (T.Text)


-- Use the python script to get sliding window fft results
-- I would rather not to this, but the Haskell one just isnt working
peakListPython :: FilePath -> OverTime (OverFreq Peak)
peakListPython audio_fp = let
  x = unsafePerformIO $ shelly $ run (fromString "python") ["fft.py", T.pack $ audio_fp]
 in
  trace (audio_fp ++ " " ++ show x) [[(0,0,0)]]

