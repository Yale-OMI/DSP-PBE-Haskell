{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Types.Filter where

import Vivid

data Filter = 
    HPF Float
  | LPF Float
  | Compose Filter Filter
  deriving (Show)

toVivid :: Filter -> (SDBody' '[] Signal -> SDBody' '[] Signal)
toVivid = \case 
    HPF t -> (\bufs -> hpf (freq_ (t::Float), in_ bufs))
    LPF t -> (\bufs -> lpf (freq_ (t::Float), in_ bufs))
    Compose f f' -> (\bufs -> do ((toVivid f).(toVivid f')) bufs)

