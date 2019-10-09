{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types.DSPNode where

import Data.Data

data DSPNode = 
    ID AmpDouble
  | HPF FreqDouble AmpDouble
  | LPF FreqDouble AmpDouble
  | PitchShift Double AmpDouble
  | FreeVerb FreqDouble Double AmpDouble
  | WhiteNoise AmpDouble
  deriving (Show, Eq, Ord, Data)

type FreqDouble = Double 
type AmpDouble = Double 

freqScale :: FreqDouble -> Float
freqScale (x) = realToFrac $ ((x+1)*10000)+100  -- freq operations 100<x<16k Hz

freqScalePitchShift x = realToFrac $ x*2000  -- pitchshift -2000<x<2000 Hz
invFreqScale x = ((x-100)/10000)-1

ampScale :: AmpDouble -> Float
ampScale (x) = realToFrac $ (x+1)/2           

delayScale x = realToFrac $ (x+1)/10             -- delay operations 0<x<.2

getParams :: DSPNode -> [(String,Double)]
getParams = \case
  ID (a) -> [("Id_app",a)]
  HPF (t) (a) -> [("HPF_thres",t), ("HPF_app",a)]
  LPF (t) (a) -> [("LPF_thres",t), ("LPF_app",a)]
  PitchShift t (a) -> [("PitchShift_thres",t), ("PitchShift_app",a)]
  FreeVerb (f) d (a) -> [("FreeVerb_freq",f), ("FreeVerb_delay",d), ("FreeVerb_app",a)]
  WhiteNoise (a) -> [("WhiteNoise_app",a)]

-- Equivelence of nodes modulo parameters
sameConstructor :: DSPNode -> DSPNode -> Bool
sameConstructor l r = toConstr l == toConstr r

data DSPNodeL = DSPNodeL{
   nodeId :: Int,
   nodeContent :: DSPNode
} deriving (Show, Eq, Ord, Data)

-- Apply the parameters of nParams to nId, to preserve the nodeId of nId
-- Throws an error if applied to nodes of different type
paramsOverNode :: DSPNodeL -> DSPNodeL -> DSPNodeL
paramsOverNode nParams nId =
   if sameConstructor (nodeContent nParams) (nodeContent nId)
   then nId{nodeContent = nodeContent nParams}
   else error $ "Cannot apply node params to a different type of node:\n"++(show $ toConstr nParams)++"\n"++(show $ toConstr nId)
