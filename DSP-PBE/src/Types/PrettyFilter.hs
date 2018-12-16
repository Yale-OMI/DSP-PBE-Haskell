{-# LANGUAGE LambdaCase #-}

module Types.PrettyFilter where

import Types.Filter
import Types.DSPNode

import qualified Data.Tree as T
import Data.Tree 

-- | A convience datatype that acts as a DSL for specifying filters
--   TODO would really like to use the OverloadedRecordFields language extension, but not sure if it works/is released
data PrettyFilter = 
    ID_p Double
  | HPF_p Double Double
  | LPF_p Double Double
  | PitchShift_p Double Double
  | Ringz_p Double Double Double
  | WhiteNoise_p Double
  | AmpApp_p Double PrettyFilter
  | ParallelCompose_p PrettyFilter PrettyFilter
  | SequentialCompose_p PrettyFilter PrettyFilter

-- | This enforces a shape on the PrettyFilter so that no left branchs may be nested
-- | TODO relax this PrettyFilter shape restriction
toInternalFilter :: PrettyFilter -> Filter
toInternalFilter = toInternalFilter' 0 
toInternalFilter' :: Int -> PrettyFilter -> Filter
toInternalFilter' counter = \case 
  AmpApp_p a f             -> T.Node {rootLabel = DSPNodeL{nodeId = counter, nodeContent = AmpApp a}, 
                                      subForest = case f of 
                                         ParallelCompose_p _ _ -> toInternalSubForest (counter+1) f
                                         _                     -> [toInternalFilter' (counter+1) f]}
  SequentialCompose_p f f' -> T.Node {rootLabel = DSPNodeL{nodeId = counter, nodeContent = toInternalDSPNode f}, 
                                      subForest = case f' of 
                                         ParallelCompose_p _ _ -> toInternalSubForest (counter+1) f'
                                         _                     -> [toInternalFilter' (counter+1) f']}
  ParallelCompose_p _ _    -> error "Found an unexpected ParallelCompose"
  n                        -> T.Node {rootLabel = DSPNodeL{nodeId = counter, nodeContent = toInternalDSPNode n}, subForest = []}

toInternalSubForest :: Int -> PrettyFilter -> [Filter]
toInternalSubForest counter = \case
  ParallelCompose_p f f'   -> [T.Node {rootLabel = DSPNodeL{nodeId = counter, nodeContent = toInternalDSPNode f}, subForest = []}] ++
                               case f' of
                                 ParallelCompose_p _ _ -> toInternalSubForest (counter+1) f'
                                 _                     -> [toInternalFilter' (counter+1) f']
  _ -> error "Bad structure"

toInternalDSPNode :: PrettyFilter -> DSPNode
toInternalDSPNode = \case
  ID_p a           -> ID a
  HPF_p t a        -> HPF t a
  LPF_p t a        -> LPF t a
  PitchShift_p t a -> PitchShift t a
  WhiteNoise_p a   -> WhiteNoise a
  Ringz_p f d a    -> Ringz f d a
  _              -> error "PrettyFilter has incorrect structure, ensure all _left_ branches are DSPNodes and not nested filters."

-- TODO, I think this might be a useful function?
fromInternalDSPNode = undefined

