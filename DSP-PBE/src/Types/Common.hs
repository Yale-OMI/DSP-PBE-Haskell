module Types.Common where

import Data.Audio
import Data.Int

type AuralDistance = Double
type AudioFormat = Audio Int16 

type OverTime = []
type OverFreq = []

-- | (freq, amp, phase)
--   TODO migrate to data type 
type Peak = (Int,Double) 


--gets 1st item of triple
getFreq :: (a,b) -> a
getFreq (a,_) = a
--gets 2nd item of triple
getAmp :: Peak -> Double
getAmp (_,amp) = amp
