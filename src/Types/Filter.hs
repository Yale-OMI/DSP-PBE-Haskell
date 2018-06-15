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
  | Ringz Float Float Float
  | WhiteNoise Float
  | Compose Filter Filter
  | AmpApp Float Filter

-- | initial points for thetas in GD
--   this can have a large impact on the effectivness of learning
--   might consider randomize restart as well
--   TODO: init value depend on the refinement type inference results
initFilter :: Thetas
initFilter = (Thetas {
  _lpfThreshold=(-0.8), _lpfApp=(1),
  _hpfThreshold=(-1), _hpfApp=(-1),
  _ringzFreq=1, _ringzDecaySecs=1, _ringzApp=(-1),
  _whiteApp=(-1),
  _ampApp=1})


instance Show Filter where
  show (Compose f f') = (show f) ++ (" >>> \n") ++ (show f')
  show (AmpApp x f) = "SetVolume: " ++ (printf "%.2f" x) ++ "% \n" ++ show f
  show (WhiteNoise x) = "WhiteNoise: " ++ (showAmp x) ++ "% "
  show (HPF fq a) = "HiPass: "++ (showFreq fq) ++ " " ++ (showAmp a)
  show (LPF fq a) = "LoPass: "++ (showFreq fq) ++ " " ++ (showAmp a)
  show (Ringz fq1 fq2 a) = "Ringz..."

freqScale x = ((x+1)*10000)+100  -- freq operations 100<x<16k Hz
ampScale x = (x+1)/2.2           -- amp operations 0<x<.9 so there is always space to explore 'up' for derivative calcuation

showAmp amp = "amp@"++(printf "%.2f" $ ampScale amp)
showFreq freq = "freq@" ++ (printf "%.0f" $ freqScale freq) 


--TODO merge with Filter
-- all these are bounded with -1<x<1, and scaled back later
data Thetas = Thetas{
   _lpfThreshold   :: Double,
   _hpfThreshold   :: Double,
   _ringzFreq      :: Double,
   _ringzDecaySecs :: Double,

   _lpfApp         :: Double, -- how much should this effect be applied
   _hpfApp         :: Double, -- allows us to SGD over a smoother space
   _ringzApp       :: Double,
   _whiteApp       :: Double,
   _ampApp         :: Double
   } deriving (Eq,Show,Generic,Hashable)

makeLenses ''Thetas

thetaDiff :: Thetas -> Thetas -> Double
thetaDiff t t' = let
  d sel = abs((sel t) - (sel t'))
 in
  sum $ map d [
	    _lpfThreshold
	  , _hpfThreshold
	  , _ringzFreq
	  , _ringzDecaySecs
	  , _lpfApp
	  , _hpfApp
	  , _whiteApp
	  , _ampApp
	  , _ringzApp]

type ResCache = H.HashMap Thetas Double

-- Given two thetas, which field did we change
thetaFieldChange t t' = if
  | _lpfThreshold t - _lpfThreshold t' /= 0 -> "lpfThreshold"
  | _hpfThreshold t - _hpfThreshold t' /= 0 -> "hpfThreshold"
  | _lpfApp       t - _lpfApp      t' /= 0 -> "lpfApp"
  | _hpfApp       t - _hpfApp      t' /= 0 -> "hpfApp"
  | _whiteApp     t - _whiteApp    t' /= 0 -> "whiteApp"
  | _ampApp       t - _ampApp      t' /= 0 -> "ampApp"
  | otherwise                              -> "unsupported field change"

--TODO this is a fixed form for now, which is fine as long as the filter are associative
--later we should be able to generate different applciation orders
thetaToFilter :: Thetas -> Filter
thetaToFilter t = AmpApp (realToFrac $ _ampApp t) $
  Compose 
     (LPF (realToFrac $ _lpfThreshold t) (realToFrac $ _lpfApp t))
     (Compose
       (HPF (realToFrac $ _hpfThreshold t) (realToFrac $ _hpfApp t))
   --    (Compose
   --      (Ringz (realToFrac $ _ringzFreq t) (realToFrac $ _ringzDecaySecs t) (realToFrac $ _ringzApp t))
         (WhiteNoise (realToFrac $ _whiteApp t))
   --    )
     )

--implements feature scaling so during GD our thetas are -1<t<1
--we onyl scale them back to the appropriate values when we need to apply theatas in a filter
toVivid :: Filter -> (SDBody' '[] Signal -> SDBody' '[] Signal)
toVivid = let
   delayScale x = (x+1)             -- delay operations 0<x<2
 in \case 
      HPF t a        -> (\bufs -> (ampScale a::Float) ~* hpf (freq_ (freqScale t::Float), in_ bufs))
      LPF t a        -> (\bufs -> (ampScale a::Float) ~* lpf (freq_ (freqScale t::Float), in_ bufs))
      WhiteNoise a   -> (\bufs -> (ampScale a::Float) ~* whiteNoise) 
      Ringz f d a    -> (\bufs -> (ampScale a::Float) ~* ringz (freq_ (freqScale f::Float), decaySecs_ (delayScale d::Float), in_ bufs))
      Compose f f'   -> (\bufs -> do (((toVivid f) bufs) ~+ ((toVivid f') bufs)))
      AmpApp a f     -> (\bufs -> do (((a+2)/3) ~* ((toVivid f) bufs))) -- can only turn down the total vol down to 1/3

