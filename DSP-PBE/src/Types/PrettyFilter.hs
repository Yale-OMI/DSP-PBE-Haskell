{-# LANGUAGE LambdaCase #-}

module Types.PrettyFilter where

import Types.Filter
import Types.DSPNode

import qualified Data.Tree as T
import Data.Tree 

-- | A convience datatype that acts as a DSL for specifying filters
data PrettyFilter = 
    Node_p DSPNode
  | ID_p Double PrettyFilter 
  | ParallelCompose PrettyFilter PrettyFilter
  | SequentialCompose PrettyFilter PrettyFilter

asDSPNode :: PrettyFilter -> DSPNode
asDSPNode f = case f of
  Node_p d -> d
  _       -> error "bad call"

-- | This enforces a shape on the PrettyFilter so that no left branchs may be nested
--   TODO relax this PrettyFilter shape restriction?
--   TODO write test to check x = toInternalFilter $ fromInternalFilter x
toInternalFilter :: PrettyFilter -> Filter
toInternalFilter = toInternalFilter' 0 
toInternalFilter' :: Int -> PrettyFilter -> Filter
toInternalFilter' counter = \case 
  ID_p a f             -> T.Node {rootLabel = DSPNodeL{nodeId = counter, nodeContent = ID a}, 
                                      subForest = case f of 
                                         ParallelCompose _ _ -> toInternalSubForest (counter+1) f
                                         _                     -> [toInternalFilter' (counter+1) f]}
  SequentialCompose f f' -> T.Node {rootLabel = DSPNodeL{nodeId = counter, nodeContent = asDSPNode f}, 
                                      subForest = case f' of 
                                         ParallelCompose _ _ -> toInternalSubForest (counter+1) f'
                                         _                     -> [toInternalFilter' (counter+1) f']}
  ParallelCompose _ _    -> error "Found an unexpected ParallelCompose"
  n                        -> T.Node {rootLabel = DSPNodeL{nodeId = counter, nodeContent = asDSPNode n}, subForest = []}

toInternalSubForest :: Int -> PrettyFilter -> [Filter]
toInternalSubForest counter = \case
  ParallelCompose f f'   -> [T.Node {rootLabel = DSPNodeL{nodeId = counter, nodeContent = asDSPNode f}, subForest = []}] ++
                               case f' of
                                 ParallelCompose _ _ -> toInternalSubForest (counter+1) f'
                                 _                     -> [toInternalFilter' (counter+1) f']
  _ -> error "Bad structure"

fromInternalFilter :: Filter -> PrettyFilter
fromInternalFilter f = let 
  dspNode = nodeContent $ rootLabel f
 in
  case dspNode of
    ID a -> ID_p a $ 
      case subForest f of
        []   -> error "ID must have subforest" 
        [d]  -> SequentialCompose (Node_p dspNode) $ fromInternalFilter d
        d:ds -> foldl (\accum n -> ParallelCompose (fromInternalFilter n) accum) (fromInternalFilter d) ds
    dspNode -> 
      case subForest f of 
        []   -> Node_p dspNode
        [d]  -> SequentialCompose (Node_p dspNode) $ fromInternalFilter d
        d:ds -> foldl (\accum n -> ParallelCompose (fromInternalFilter n) accum) (fromInternalFilter d) ds

