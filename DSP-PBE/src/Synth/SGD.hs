{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BangPatterns #-}

module Synth.SGD where

import System.Random.Shuffle
import System.Random

import qualified Data.HashMap.Strict as H
import Data.List
import Data.Ord

import GHC.Generics
import Control.Lens
import Control.Monad

import Types.Filter
import Types.Thetas
import qualified Settings as S

import Utils

-- | Use data type Theta for params to be passed to eval fxn
--   calc new theta and build map of theta to value to avoid recomputation
multiVarSGD :: RandomGen g => 
  _ ->        -- ^ selectors for all dimension of theta
  g ->        -- ^ a random generator for the Stochastic-ness of SGD
  Int ->      -- ^ the size of each batch in SGD
  Double ->   -- ^ the goal for covergence - what do we condsider to be a negligible gain from one step of SGD
  Double ->   -- ^ the learning rate, this is not a global setting as some SGD implementations use a dynamic learning rate
  Thetas ->   -- ^ the current theta, from which we will descend the gradient
  (Thetas -> IO Double) -> 
  ResCache ->
  IO Thetas
multiVarSGD thetaSelectors g batchSize goal !learnRate !currentTheta costFxn currentCache = do

  -- in SGD, in each round, we randomly (g) select a few (batchSize) dimensions (thetaSelectors) to descend on
  let randSelectors = stochasticBatch g batchSize thetaSelectors

  -- In GD, we must use a fixed theta (t) for updating each dimension (takeStep) of theta,
  -- and aggregate those these updates in a seperate copy of theta (updatesThetas) by using the fold
  steppedThetas  <- foldM (takeStep learnRate currentTheta costFxn) currentTheta randSelectors

  steppedScore <- costFxn steppedThetas


  let 
    newCache = H.insert steppedThetas steppedScore currentCache
    -- build the call to try again using updatedThetas, allowing us to explore worse directions, but every n step returning to best
    continueGD = multiVarSGD thetaSelectors (snd $ next g) batchSize goal learnRate steppedThetas costFxn newCache
    -- converge if we try to descend and still find the same best thetas (within the goal threshold)
    converged = thetaDiff steppedThetas currentTheta <= goal || H.member steppedThetas currentCache 

  debugPrint $ "Score for this step is "++(show steppedScore)
  
  if not converged 
  then (trace "\n" continueGD)
  else do
    let (bestThetas, bestScore) = getMinScore currentCache
    debugPrint ("\n\n\nFinished SGD with score = "++(show bestScore)
                  ++"\nUsing Theta: "++ (indent $ show $ thetaToFilter bestThetas))
    return bestThetas

getMinScore :: ResCache -> (Thetas, Double)
getMinScore cache = 
  H.foldlWithKey' (\(bestT,bestS) t s -> if bestS > s then (t,s) else (bestT,bestS)) (initThetas, read "Infinity") cache
  -- or, a more clear, but less efficent version
  -- minimumBy (comparing snd) $ H.toList cache 


-- TODO make sure we always take the thetas that were the most effective in the previous step
stochasticBatch :: RandomGen g => g -> Int -> [a] -> [a]
stochasticBatch g batchSize xs =
  take batchSize $ shuffle' xs (length xs) g

-- | descend by a single step in the direction of the largest gradient over a single dimension
takeStep :: 
  Double -> -- ^ the learning rate
  Thetas -> 
  (Thetas -> IO Double) 
  -> Thetas
  -> _ 
  -> IO Thetas
takeStep learnRate t f updatedTheta part = do 
  slope <- partialDerivative f part t
  let newTheta = over part (boundedUpdate learnRate slope) updatedTheta --not allowed to move more than 0.2 in a single step
  debugPrint ("Adjusting "++(thetaFieldChange newTheta updatedTheta)++" by "++(show (thetaDiff updatedTheta newTheta)))
  debugPrint ("Scoring program...\n"++(indent $ show $ thetaToFilter newTheta))
  debugPrint ""
  return newTheta

-- | never move outside [-1,1] and
--   never update by more than [-0.2,0.2] in a single step
boundedUpdate learnRate slope prevVal  = let
  proposedStep = learnRate * slope
  boundedStep = max (-0.2) $ min (0.2) proposedStep
 in  
  -- (-) here because if the tangent has positive slope (increasing prevVal increases cost), we need to go in the opposite direction
  max (-1) $ min 1 $ (prevVal + boundedStep) 

-- | Calculate the derivative of one dimension of the theta
--   since we cannot calculate the derivative of the cost function, we just approximate the deritative with a small step
--   this is an inefficent method (althought the caching helps) - other options are listed in Synth/Synth.hs
partialDerivative  :: 
     (Thetas -> IO Double) -- ^ distance metric
  -> _                     -- ^ dimension selector
  -> Thetas                -- ^ current theta to take partial derivative of
  -> IO Double -- ^ derivative and updated cache
partialDerivative f part t = do
  -- derivative approximation step size TODO, move to settings?
  let s = 0.01 

  -- score of current theta
  scoreOrig <- f t
  debugPrint $ "Calculating Partial Derivative wrt "++(thetaFieldChange t (over part (\x -> x+s) t))
  -- score of theta with small movement in dimension of interest
  scoreDelta <- f (over part (\x -> x+s) t)

  -- rise over run to find slope 
  let adjustment = (scoreOrig-scoreDelta)/s
  debugPrint $ "Derivative in "++(thetaFieldChange t (over part (\x -> x+s) t))++" = "++(show adjustment)
  return $ adjustment
