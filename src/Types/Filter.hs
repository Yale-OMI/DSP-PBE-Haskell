{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE RecordWildCards #-}

module Types.Filter where

import Vivid

import qualified Data.HashMap.Strict as H
import Data.Hashable

import GHC.Generics
import Control.Lens

import Debug.Trace

data Filter = 
    HPF Float Float
  | LPF Float Float
  | Ringz Float Float Float
  | WhiteNoise Float
  | Compose Filter Filter
  | AmpApp Float Filter
  deriving (Show)

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


--TODO this is a fixed form for now, which is fine as long as the filter are associative
--later we should be able to generate different applciation orders
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


--TODO need feature scaling
toVivid :: Filter -> (SDBody' '[] Signal -> SDBody' '[] Signal)
toVivid = let
   freqScale x = (x+1)*10000  -- freq operations 0<x<20k Hz
   ampScale x = (x+1)/2.2       -- amp operations 0<x<.9 so there is always space to explore 'up' for derivative calcuation
   delayScale x = (x+1)         -- delay operations 0<x<2
 in \case 
      HPF t a  -> (\bufs -> (ampScale a::Float) ~* hpf (freq_ (freqScale t::Float), in_ bufs))
      LPF t a  -> (\bufs -> (ampScale a::Float) ~* lpf (freq_ (freqScale t::Float), in_ bufs))
      WhiteNoise a -> traceShow ("whiteNoise("++(show $ ampScale a)++")") (\bufs -> ((ampScale a::Float) ~* whiteNoise) ~+ bufs)
      Ringz f d a -> (\bufs -> (ampScale a::Float) ~* ringz (freq_ (freqScale f::Float), decaySecs_ (delayScale d::Float), in_ bufs))
      Compose f f' -> (\bufs -> do ((toVivid f) bufs ~+ (toVivid f') bufs))
      AmpApp a f -> (\bufs -> do (((a+2)/3) ~* ((toVivid f) bufs))) -- can only turn down the total vol down to 1/3

