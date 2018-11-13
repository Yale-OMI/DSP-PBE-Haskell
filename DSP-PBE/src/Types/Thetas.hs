{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE MultiWayIf #-}

module Types.Thetas where

import qualified Data.HashMap.Strict as H
import Data.Hashable

import GHC.Generics
import Control.Lens

import Types.Filter

import Debug.Trace

-- all these are bounded with -1<x<1, and scaled back later
-- This structure only allows for a single application of each filter
-- possible TODO, allow mulitple application of a filter (eg HPF) in a program
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
   _ampApp         :: Double,
   _idApp          :: Double
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
         , _idApp
         , _ringzApp]

type ThetaLog = H.HashMap Thetas Double

-- | initial points for thetas in GD
--   this can have a large impact on the effectivness of learning
--   might consider randomize restart as well
--   TODO: init value depend on the refinement type inference results
initThetas :: Thetas
initThetas = (Thetas {
  _lpfThreshold=(-0.8)    , _lpfApp=(1)        ,
  _hpfThreshold=(-1)      , _hpfApp=(-1)       ,
  _pitchShiftFreq =(-0.8) , _pitchShiftApp=(1) ,
  _ringzFreq=(-0.1)       , _ringzDecaySecs=0  , _ringzApp=((-0.5)) ,
  _whiteApp=(-1)          ,
  _idApp=(1)          ,
  _ampApp=1})


-- Given two thetas, which field did we change
thetaFieldChange t t' = if
  | _lpfThreshold   t - _lpfThreshold   t' /= 0 -> "lpfThreshold"
  | _hpfThreshold   t - _hpfThreshold   t' /= 0 -> "hpfThreshold"
  | _pitchShiftFreq t - _pitchShiftFreq t' /= 0 -> "pitchShiftFreq"
  | _lpfApp         t - _lpfApp         t' /= 0 -> "lpfApp"
  | _hpfApp         t - _hpfApp         t' /= 0 -> "hpfApp"
  | _pitchShiftApp  t - _pitchShiftApp  t' /= 0 -> "pitchShiftApp"
  | _whiteApp       t - _whiteApp       t' /= 0 -> "whiteApp"
  | _ampApp         t - _ampApp         t' /= 0 -> "ampApp"
  | _idApp          t - _idApp          t' /= 0 -> "idApp"
  | _ringzFreq      t - _ringzFreq      t' /= 0 -> "ringzFreq"
  | _ringzDecaySecs t - _ringzDecaySecs t' /= 0 -> "ringzDecaySecs"
  | _ringzApp       t - _ringzApp       t' /= 0 -> "ringzApp"
  | t == t'                                     -> "No change"
  | otherwise                                   -> "unsupported field change"

-- | we only care about equality of theta up to equality on filters
--   if two thetas are different in a way that cannot be expressed in vivid, 
--   it doesnt matter that they are different
--   we choose the default fitler structure here since what we really care about are the theta value conversions
--   TODO should get rid of thetaToFilter and inspect value directly
instance Eq Thetas where
  (==) a b = thetaToFilter a == thetaToFilter b

thetaToFilter :: Thetas -> Filter
thetaToFilter t = AmpApp (realToFrac $ _ampApp t) $
  ParallelCompose 
   (LPF (realToFrac $ _lpfThreshold t) (realToFrac $ _lpfApp t))
   (ParallelCompose
       (HPF (realToFrac $ _hpfThreshold t) (realToFrac $ _hpfApp t))
     (ParallelCompose
       (PitchShift (realToFrac $ _pitchShiftFreq t) (realToFrac $ _pitchShiftApp t))
       (ParallelCompose
         (Ringz (realToFrac $ _ringzFreq t) (realToFrac $ _ringzDecaySecs t) (realToFrac $ _ringzApp t))
         (WhiteNoise (realToFrac $ _whiteApp t))
       )
     )
   )

-- Map the values of the given theta over the fitler
thetaOverFilter :: Filter -> Thetas -> Filter
thetaOverFilter filterStructure ts@Thetas{..} = case filterStructure of
      ID a                   -> ID (rtf _idApp)
      HPF t a                -> HPF (rtf _hpfThreshold) (rtf _hpfApp)
      LPF t a                -> LPF (rtf _lpfThreshold) (rtf _lpfApp)
      PitchShift t a         -> PitchShift (rtf _pitchShiftFreq) (rtf _pitchShiftApp)
      WhiteNoise a           -> WhiteNoise (rtf _whiteApp)
      Ringz t d a            -> Ringz (rtf _ringzFreq) (rtf _ringzDecaySecs) (rtf _ringzApp)
      AmpApp a f             -> AmpApp (rtf _ampApp) (thetaOverFilter f ts)
      ParallelCompose f f'   -> ParallelCompose (thetaOverFilter f ts) (thetaOverFilter f' ts)
      SequentialCompose f f' -> SequentialCompose (thetaOverFilter f ts) (thetaOverFilter f' ts)
    where
  rtf = realToFrac

-- | Extract the thetas from a filter
--   This loses the strucutal information of the program
filterToThetas :: Filter -> Thetas
filterToThetas filter = filterToThetas' filter blankThetas

filterToThetas' :: Filter -> Thetas -> Thetas
filterToThetas' filter thetasAccum@Thetas{..} = case filter of 
    ID a                   -> thetasAccum {_idApp=rtf a}
    HPF t a                -> thetasAccum {_hpfThreshold=rtf t, _hpfApp=rtf a}
    LPF t a                -> thetasAccum {_lpfThreshold=rtf t, _lpfApp=rtf a}
    PitchShift t a         -> thetasAccum {_pitchShiftFreq=rtf t, _pitchShiftApp=rtf a}
    WhiteNoise a           -> thetasAccum {_whiteApp=rtf a}
    Ringz t d a            -> thetasAccum {_ringzFreq=rtf t, _ringzDecaySecs=rtf d, _ringzApp=rtf a}
    AmpApp a f             -> (filterToThetas' f thetasAccum) {_ampApp=rtf a}
    ParallelCompose f f'   -> mergeByMax (filterToThetas' f thetasAccum) (filterToThetas' f' thetasAccum)
    SequentialCompose f f' -> mergeByMax (filterToThetas' f thetasAccum) (filterToThetas' f' thetasAccum)
 where
  rtf = realToFrac

-- TODO might be nice for Thetas to be Traversable
-- could eliminate a lot of this boilerplate
mergeByMax :: Thetas -> Thetas -> Thetas
mergeByMax t1 t2 = Thetas {
    _lpfThreshold   = max ( _lpfThreshold   t1 ) ( _lpfThreshold   t2 )
  , _lpfApp         = max ( _lpfApp         t1 ) ( _lpfApp         t2 )
  , _hpfThreshold   = max ( _hpfThreshold   t1 ) ( _hpfThreshold   t2 )
  , _hpfApp         = max ( _hpfApp         t1 ) ( _hpfApp         t2 )
  , _idApp          = max ( _idApp          t1 ) ( _idApp          t2 )
  , _pitchShiftFreq = max ( _pitchShiftFreq t1 ) ( _pitchShiftFreq t2 )
  , _pitchShiftApp  = max ( _pitchShiftApp  t1 ) ( _pitchShiftApp  t2 )
  , _ringzFreq      = max ( _ringzFreq      t1 ) ( _ringzFreq      t2 )
  , _ringzDecaySecs = max ( _ringzDecaySecs t1 ) ( _ringzDecaySecs t2 )
  , _ringzApp       = max ( _ringzApp       t1 ) ( _ringzApp       t2 )
  , _whiteApp       = max ( _whiteApp       t1 ) ( _whiteApp       t2 )
  , _ampApp         = max ( _ampApp         t1 ) ( _ampApp         t2 ) }

-- start with an 'empty' theta structure so we can mergeByMax
-- TODO I should migrate the Thetas fields to Maybe Double, but this is fine
blankThetas :: Thetas
blankThetas = (Thetas {
  _lpfThreshold   = (-1) , _lpfApp         = (-1) ,
  _hpfThreshold   = (-1) , _hpfApp         = (-1) ,
  _pitchShiftFreq = (-1) , _pitchShiftApp  = (-1) ,
  _ringzFreq      = (-1) , _ringzDecaySecs = (-1) , _ringzApp = (-1) ,
  _whiteApp       = (-1) ,
  _idApp          = (-1) ,
  _ampApp         = (-1)})


