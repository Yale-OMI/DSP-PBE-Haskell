{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE MultiWayIf #-}

module Types.Thetas where

import qualified Data.HashMap.Strict as H
import Data.Hashable

import GHC.Generics
import Control.Lens

import Types.Filter

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

-- | we only care about equality of theta up to equality on filters
--   if two thetas are different in a way that cannot be expressed in vivid, 
--   it doesnt matter that they are different
instance Eq Thetas where
  (==) a b = thetaToFilter a == thetaToFilter b

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


