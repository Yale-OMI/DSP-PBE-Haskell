module Types.Common where

import Data.Audio
import Data.Int

type AuralDistance = Double
type AudioFormat = Audio Int16 


-- (freq, amp, phase)
type Peak = (Int,Double,Double) 
