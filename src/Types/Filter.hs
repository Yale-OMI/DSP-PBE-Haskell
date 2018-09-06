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

-- | initial points for thetas in GD
--   this can have a large impact on the effectivness of learning
--   might consider randomize restart as well
--   TODO: init value depend on the refinement type inference results
initFilter :: Thetas
initFilter = (Thetas {
  _lpfThreshold=(-0.8), _lpfApp=(1),
  _hpfThreshold=(-1), _hpfApp=(-1),
  _pitchShiftFreq =(-0.8), _pitchShiftApp=(1),
  _ringzFreq=(-0.1), _ringzDecaySecs=0, _ringzApp=((-0.5)),
  _whiteApp=(-1),
  _ampApp=1})


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


--TODO merge with Filter
-- all these are bounded with -1<x<1, and scaled back later
data Thetas = Thetas{
   _lpfThreshold   :: Double,
   _hpfThreshold   :: Double,
   _pitchShiftFreq :: Double,
   _ringzFreq      :: Double,
   _ringzDecaySecs :: Double,

   _lpfApp         :: Double, -- how much should this effect be applied
   _hpfApp         :: Double, -- allows us to SGD over a smoother space
   _pitchShiftApp  :: Double,
   _ringzApp       :: Double,
   _whiteApp       :: Double,
   _ampApp         :: Double
   } deriving (Show,Generic,Hashable)

makeLenses ''Thetas

thetaDiff :: Thetas -> Thetas -> Double
thetaDiff t t' = let
  d sel = abs((sel t) - (sel t'))
 in
  sum $ map d [
           _lpfThreshold
         , _hpfThreshold
         , _pitchShiftFreq
         , _ringzFreq
         , _ringzDecaySecs
         , _lpfApp
         , _hpfApp
         , _pitchShiftApp
         , _whiteApp
         , _ampApp
         , _ringzApp]

type ResCache = H.HashMap Thetas Double

-- Given two thetas, which field did we change
thetaFieldChange t t' = if
  | _lpfThreshold t - _lpfThreshold t' /= 0 -> "lpfThreshold"
  | _hpfThreshold t - _hpfThreshold t' /= 0 -> "hpfThreshold"
  | _pitchShiftFreq t - _pitchShiftFreq t' /= 0 -> "pitchShiftFreq"
  | _lpfApp       t - _lpfApp      t' /= 0 -> "lpfApp"
  | _hpfApp       t - _hpfApp      t' /= 0 -> "hpfApp"
  | _pitchShiftApp t - _pitchShiftApp t' /= 0 -> "pitchShiftApp"
  | _whiteApp     t - _whiteApp    t' /= 0 -> "whiteApp"
  | _ampApp       t - _ampApp      t' /= 0 -> "ampApp"
  | _ringzFreq    t - _ringzFreq      t' /= 0 -> "ringzFreq"
  | _ringzDecaySecs t - _ringzDecaySecs      t' /= 0 -> "ringzDecaySecs"
  | _ringzApp     t - _ringzApp t' /= 0    -> "ringzApp"
  | t == t'                                -> "No change"
  | otherwise                              -> "unsupported field change"

--TODO this is a fixed form for now, which is fine as long as the filter are associative
--later we should be able to generate different applciation orders
thetaToFilter :: Thetas -> Filter
thetaToFilter t = AmpApp (realToFrac $ _ampApp t) $
  Compose 
     (LPF (realToFrac $ _lpfThreshold t) (realToFrac $ _lpfApp t))
     (Compose
       (HPF (realToFrac $ _hpfThreshold t) (realToFrac $ _hpfApp t))
     (Compose
       (PitchShift (realToFrac $ _pitchShiftFreq t) (realToFrac $ _pitchShiftApp t))
       (Compose
         (Ringz (realToFrac $ _ringzFreq t) (realToFrac $ _ringzDecaySecs t) (realToFrac $ _ringzApp t))
         (WhiteNoise (realToFrac $ _whiteApp t))
       )
     ))


-- | we only care about equality of theta up to equality on filters
--   if two thetas are different in a way that cannot be expressed in vivid, 
--   it doesnt matter that they are different
instance Eq Thetas where
  (==) a b = thetaToFilter a == thetaToFilter b

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

