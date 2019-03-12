module Synth.StructuralRefinement where

import qualified Settings as S
import Types.Filter
import Types.PrettyFilter
import Types.DSPNode

import Data.List
import System.Exit
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Tree as T

import Debug.Trace

structuralRefinement :: S.Options -> Filter -> FilterLog -> Filter
structuralRefinement settings prevFilter fLog =
  if S.smartStructuralRefinement settings
  then smartRefine prevFilter fLog
  else bruteForceRefine fLog

bruteForceRefine :: FilterLog -> Filter
bruteForceRefine fLog  =
  toInternalFilter $ head $ drop (M.size fLog) allFilters

cores :: [PrettyFilter]
cores = map Node_p
  [ ID 0
  , HPF 0 0 
  , LPF 0 0 
  , PitchShift 0 0 
  , Ringz 0 0 0 
  , WhiteNoise 0 
  ]

allCores :: [[PrettyFilter]]
allCores = permutations cores

type FilterOp = PrettyFilter -> PrettyFilter -> PrettyFilter

composers :: [FilterOp]
composers =
  [ ParallelCompose
  , SequentialCompose
  ]

allComposes :: [[FilterOp]]
allComposes = mapM (const composers) [1..(length cores - 1)]

allFilterComposePairs = [(cs,ops) | cs <- allCores, ops <- allComposes]

combine :: [PrettyFilter] -> [FilterOp] -> PrettyFilter
combine (f:fs) (op:ops) = op f $ combine fs ops
combine [f] [] = f
combine _ _ = error "tried to combine incorrect num of ops and filters"

-- Warning! Don't evaluate this with any strict operation
-- length allFitlers = 
{-  |allCores|    = (|cores|)^(|cores|)
    |allComposes| = |composes|^(|cores|-1)
    |allPairs|    = |allCores| * |composes|
    = 1,492,992
-}
allFilters = map (AmpApp_p 0) $ map (uncurry combine) allFilterComposePairs

{-
toList f = case f of
  ParallelCompose f1 f2 -> toList f2 ++ toList f1
  SequentialCompose f1 f2 -> SequentialCompose (flipOneParallel f1)
  x -> x

flipOneParallel f = case f of
  ParallelCompose f1 f2 -> ParallelCompose f2 f1
  SequentialCompose f1 f2 -> SequentialCompose (flipOneParallel f1)
  x -> x
-}

smartRefine :: Filter -> FilterLog -> Filter
smartRefine _ fLog = let 
  bestSoFar = fst $ M.findMin fLog
 in
  greedyRefine bestSoFar fLog


cores_i:: [DSPNode]
cores_i = 
  [ ID 0
  , HPF 0 0 
  , LPF 0 0 
  , PitchShift 0 0 
  , Ringz 0 0 0 
  , WhiteNoise 0 
  ]

composers_i :: [Filter -> DSPNode -> Filter]
composers_i = 
  [(\f1 f2 -> f1{T.subForest = (pure $ DSPNodeL{nodeId = length f1+1, nodeContent =f2}):(T.subForest f1)}) -- a sort of parallel
  ,(\f1 f2 -> (pure $ DSPNodeL{nodeId = length f1+1, nodeContent = f2}){T.subForest=[f1]}) --put it at the head, a type of sequential
  ]

-- | First, find out what we haven't tried that builds off the best option so far
--   If we tried every option off the best, take the next best option and try to build off that
--   passing our entire current progress along each step of the way
greedyRefine :: Filter -> FilterLog -> Filter
greedyRefine bestSoFar fLog = let
  dspNodesUnused = filter (\c -> not $ any (sameConstructor c) $ map nodeContent $ T.flatten bestSoFar) cores_i
  nextOptions = zipWith (\composer newFilterNode -> composer bestSoFar newFilterNode) composers_i dspNodesUnused
  nextOptionsUntested = filter (\x -> not $ M.member x fLog) nextOptions
 in
  case nextOptionsUntested of 
    [] -> traceShow ("Backtracking: "++show(bestSoFar)) $ greedyRefine (fst $ M.findMin $ M.delete bestSoFar fLog) fLog
    xs -> head xs

