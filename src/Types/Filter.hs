{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Types.Filter where

import Vivid

import qualified Data.HashMap.Strict as H
import Data.Hashable

import GHC.Generics
import Control.Lens
import Control.Monad


data Filter = 
    HPF Float
  | LPF Float
  | Ringz Float
  | Compose Filter Filter
  deriving (Show)

--TODO merge with Filter
data Thetas = Thetas{
   _lpfThreshold :: Double,
   _hpfThreshold :: Double,
   _ringzFreq    :: Double,

   _lpfApp       :: Double, -- how much should this effect be applied
   _hpfApp       :: Double, -- allows us to SGD over a smoother space
   _ringzApp     :: Double
   } deriving (Eq,Show,Generic,Hashable)

makeLenses ''Thetas

type ResCache = H.HashMap Thetas Double


thetaToFilter t =
  Compose 
     (LPF $ realToFrac $ _lpfThreshold t ) 
     (Compose
       (HPF $ realToFrac $ _hpfThreshold t)
       (Ringz $ realToFrac $ _ringzFreq t))


toVivid :: Filter -> (SDBody' '[] Signal -> SDBody' '[] Signal)
toVivid = \case 
    HPF t   -> (\bufs -> hpf (freq_ (t::Float), in_ bufs)) --TODO replace 0.1 with _hpfApp
    LPF t   -> (\bufs -> lpf (freq_ (t::Float), in_ bufs))
    Ringz t -> (\bufs -> hpf (freq_ (t::Float), in_ bufs))
    Compose f f' -> (\bufs -> do ((toVivid f).(toVivid f')) bufs)

