module Synth.StructuralRefinement where

import qualified Settings as S
import Types.Filter
import Types.Thetas

import Data.List

structuralRefinement :: S.Options -> Filter -> ResCache -> Filter
structuralRefinement settings prevFilter log =
  if S.smartStructuralRefinement settings
  then smartRefine prevFilter log 
  else bruteForceRefine prevFilter

-- TODO keep a record of the attempted structures
-- for brute force we can then use
bruteForceRefine :: Filter -> Filter
bruteForceRefine f = undefined
-- head $ drop (length attemptedStructures) allFilters

cores :: [Filter]
cores = 
  [ ID 0
  , HPF 0 0 
  , LPF 0 0 
  , PitchShift 0 0 
  , Ringz 0 0 0 
  , WhiteNoise 0 
  ]

allCores :: [[Filter]]
allCores = permutations cores

type FilterOp = Filter -> Filter -> Filter

composes :: [FilterOp]
composes =
  [ ParallelCompose 
  , SequentialCompose
  ]

allComposes :: [[FilterOp]]
allComposes = mapM (const composes) [1..(length cores - 1)]

allFilterComposePairs = [(cs,ops) | cs <- allCores, ops <- allComposes]

combine :: [Filter] -> [FilterOp] -> Filter
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
allFilters = map (uncurry combine) allFilterComposePairs

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

-- TODO choose strucutres intelligently...
-- maybe based of the weights? sequence filters with high weight?
smartRefine :: Filter -> ResCache -> Filter
smartRefine f log = f
