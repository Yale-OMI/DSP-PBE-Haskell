{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types.DSPNode where

import Data.Data

data DSPNode = 
    ID Double
  | HPF Double Double
  | LPF Double Double
  | PitchShift Double Double
  | Ringz Double Double Double
  | WhiteNoise Double
  | AmpApp Double --TODO how is amp differnet from ID?
  deriving (Show, Eq, Ord, Data)

getParams :: DSPNode -> [(String,Double)]
getParams = \case
  ID a -> [("Id_app",a)]
  HPF t a -> [("HPF_thres",t), ("HPF_app",a)]
  LPF t a -> [("LPF_thres",t), ("LPF_app",a)]
  PitchShift t a -> [("PitchShift_thres",t), ("PitchShift_app",a)]
  Ringz f d a -> [("Ringz_freq",f), ("Ringz_delay",d), ("Ringz_app",a)]
  WhiteNoise a -> [("WhiteNoise_app",a)]
  AmpApp a -> [("Amp_app",a)]

-- Equivelence of ndoes modulo parameters
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
