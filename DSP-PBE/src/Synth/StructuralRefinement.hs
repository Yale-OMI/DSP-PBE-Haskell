module Synth.StructuralRefinement where

import qualified Settings as S
import Types.Filter
import Types.PrettyFilter
import Types.DSPNode
import Types.Common
import Analysis.FilterDistance

import Data.List
import System.Exit
import qualified Data.HashMap.Strict as H
import qualified Data.Map as M
import qualified Data.Tree as T

import Utils 
import Debug.Trace

structuralRefinement :: S.Options -> AudioFormat -> FilterLog -> IO Filter
structuralRefinement settings out_audio fLog =
  if S.smartStructuralRefinement settings
  then smartRefine settings out_audio fLog
  else return $ bruteForceRefine fLog

-- | brute force enumerates all filter structure and tries them one by one
bruteForceRefine :: FilterLog -> Filter
bruteForceRefine fLog  =
  toInternalFilter $ head $ drop (M.size fLog) allFilters

smartRefine :: S.Options -> AudioFormat -> FilterLog -> IO Filter
smartRefine settings outAudio fLog = let 
  bestSoFar = fst $ findMinByVal fLog
 in
  greedyRefine settings outAudio bestSoFar [] fLog

-- | First, find out what we haven't tried that builds off the best option so far
--   If we tried every option off the best, take the next best option and try to build off that
--   passing our entire current progress along each step of the way
greedyRefine :: S.Options -> AudioFormat -> Filter -> [Filter] -> FilterLog -> IO Filter
greedyRefine settings outAudio bestSoFar oldDeadEnds fLog = let
  dspNodesUnused = selectUnused bestSoFar
  
  -- try all ways to compose one new dspNode with the best choice so far for all dspNodes
  nextOptions = concatMap (\newFilterNode -> map (\c -> c bestSoFar newFilterNode) composers_i) dspNodesUnused
  
  -- filter out any structure we have already tried
  nextOptionsUntested = filter (\x -> not $ any (sameStructure x) $ getKeys fLog) nextOptions
  getKeys = map fst. M.toList
  
  nextUntestedWithVals = applyDerivedMetricalConstraints fLog nextOptionsUntested
  deadEnds = bestSoFar:oldDeadEnds
 in
  case nextUntestedWithVals of 
    [] -> traceShow ("Backtracking on: "++show(bestSoFar)) $ 
             if M.size fLog == 1
             then do 
               --TODO is there something smarter to do here? this might cause a problem with termination 
               debugPrint "Tried all the options, giving back bestSoFar" 
               return bestSoFar
             else greedyRefine settings outAudio (fst $ findMinByVal $ foldl (flip M.delete) fLog deadEnds) deadEnds fLog
    xs -> takeBestFilter settings outAudio xs

-- | which dspNodes have we not used in the filter (we should probably allow adding any nodes instead?)
selectUnused :: Filter -> [DSPNode]
selectUnused bestSoFar = 
   filter (\c -> not $ any (sameConstructor c) $ map nodeContent $ T.flatten bestSoFar) cores

-- | use the best solution in the log to find a good point for init thetas
applyDerivedMetricalConstraints :: FilterLog -> [Filter] -> [Filter]
applyDerivedMetricalConstraints fLog fs = let
  initThetas = fst $ findMinByVal fLog
  -- apply the params of f1 to the structure of f2
  filterParamsOverFilter f1 f2 = fmap (\n -> paramsOverNode n $ findSameNode f1 n) f2
 in
  map (filterParamsOverFilter initThetas) fs

-- | the greedy part of greedy refinement - given a set of options for the next filter
--   run all of them and see which is best, then take that
takeBestFilter :: S.Options -> AudioFormat -> [Filter] -> IO Filter
takeBestFilter settings outAudio fs = do
  let
    testFilter' = testFilter (S.inputExample settings) (S.outputExample settings, outAudio)
  dists <- mapM testFilter' fs
  return $ fst $ findMinByVal $ M.fromList $ zip fs dists



cores :: [DSPNode]
cores = 
  [ ID 0
  , HPF 1 0 
  , LPF 1 0 
  , PitchShift 0.1 0.1
  , Ringz 0.1 0.1 0 
  , WhiteNoise 0.1
  ]

allCores :: [[DSPNode]]
allCores = permutations cores

type FilterOp = PrettyFilter -> PrettyFilter -> PrettyFilter

composers_i :: [Filter -> DSPNode -> Filter]
composers_i = 
  [(\f1 f2 -> f1{T.subForest = (pure $ DSPNodeL{nodeId = length f1+1, nodeContent =f2}):(T.subForest f1)}) -- a sort of parallel
  ,(\f1 f2 -> (pure $ DSPNodeL{nodeId = length f1, nodeContent = f2}){T.subForest=[f1]}) --put it at the head, a type of sequential
  ]

composers :: [FilterOp]
composers =
  [ ParallelCompose
  , SequentialCompose
  ]

allComposes :: [[FilterOp]]
allComposes = mapM (const composers) [1..(length cores - 1)]

allFilterComposePairs = [(cs,ops) | cs <- allCores, ops <- allComposes]

combine :: [DSPNode] -> [FilterOp] -> PrettyFilter
combine (f:fs) (op:ops) = op (Node_p f) $ combine fs ops
combine [f] [] = Node_p f
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
