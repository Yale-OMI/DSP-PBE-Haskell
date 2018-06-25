module Types.Common where

import Data.Audio
import Data.Int

type AuralDistance = Double
type AudioFormat = Audio Int16 

type OverTime = []
type OverFreq = []

-- | (freq, amp, phase)
--   TODO migrate to data type 
type Peak = (Int,Double,Double) 

--gets 1st item of triple
getFreq :: (a,b,c) -> a
getFreq (a,_,_) = a
--gets 2nd item of triple
getAmp :: Peak -> Double
getAmp (_,amp,_) = amp
