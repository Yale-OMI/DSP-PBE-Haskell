{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Types.Filter where

import Vivid

import qualified Data.HashMap.Strict as H
import Data.Hashable

import GHC.Generics
import Control.Lens

import Text.Printf

import Debug.Trace

data Filter = 
    HPF Float Float
  | LPF Float Float
  | PitchShift Float Float
  | Ringz Float Float Float
  | WhiteNoise Float
  | Compose Filter Filter
  | AmpApp Float Filter
  deriving (Eq)

instance Show Filter where
  show (Compose f f') = (show f) ++ (" >>> \n") ++ (show f')
  show (AmpApp x f) = "SetVolume: " ++ (printf "%.2f" x) ++ "% \n" ++ show f
  show (WhiteNoise x) = "WhiteNoise: " ++ (showAmp x) ++ "% "
  show (HPF fq a) = "HiPass: "++ (showFreq fq) ++ " " ++ (showAmp a)
  show (LPF fq a) = "LoPass: "++ (showFreq fq) ++ " " ++ (showAmp a)
  show (PitchShift fq a) = "PitchShift: "++ (showFreqP fq) ++ " " ++ (showAmp a)
  show (Ringz fq d a) = "Ringz: "++ (showFreq fq) ++ " " ++ (showDelay d) ++ " " ++ (showAmp a)

freqScale x = ((x+1)*10000)+100  -- freq operations 100<x<16k Hz
freqScalePitchShift x = x*2000  -- pitchshift -2000<x<2000 Hz
invFreqScale x = ((x-100)/10000)-1
ampScale x = (x+1)/2.2           -- amp operations 0<x<.9 so there is always space to explore 'up' for derivative calcuation
delayScale x = (x+1)/10             -- delay operations 0<x<.2

showAmp amp = "amp@"++(printf "%.2f" $ ampScale amp)
showFreq freq = "freq@" ++ (printf "%.0f" $ freqScale freq)
showFreqP freq = "freq@" ++ (printf "%.0f" $ freqScalePitchShift freq)
showDelay d = "delay@" ++ (printf "%.2f" $ delayScale d)

--implements feature scaling so during GD our thetas are -1<t<1
--we onyl scale them back to the appropriate values when we need to apply theatas in a filter
toVivid :: Filter -> (SDBody' '[] Signal -> SDBody' '[] Signal)
toVivid = \case 
      HPF t a        -> (\bufs -> (ampScale a::Float) ~* hpf (freq_ (freqScale t::Float), in_ bufs))
      LPF t a        -> (\bufs -> (ampScale a::Float) ~* lpf (freq_ (freqScale t::Float), in_ bufs))
      PitchShift t a -> (\bufs -> (ampScale a::Float) ~* freqShift (freq_ (freqScalePitchShift t::Float), in_ bufs)) -- there is also pitchShift in vivid, but it is more complex
      WhiteNoise a   -> (\bufs -> (ampScale a::Float) ~* whiteNoise) 
      Ringz f d a    -> (\bufs -> (ampScale a::Float) ~* ringz (freq_ (freqScale f::Float), decaySecs_ (delayScale d::Float), in_ bufs))
      Compose f f'   -> (\bufs -> do (((toVivid f) bufs) ~+ ((toVivid f') bufs)))
      AmpApp a f     -> (\bufs -> do (((a+2)/3) ~* ((toVivid f) bufs))) -- can only turn down the total vol down to 1/3

