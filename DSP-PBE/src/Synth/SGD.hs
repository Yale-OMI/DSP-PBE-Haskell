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
  S.Options ->
  _ ->        -- ^ selectors for all dimension of theta
  (Thetas -> IO Double) -> 
  g ->        -- ^ a random generator for the Stochastic-ness of SGD
  ResCache ->
  Thetas ->   -- ^ the current theta, from which we will descend the gradient
  IO (Thetas, Double, ResCache) -- ^ returns the solution, the cost, and the log of all attempts
multiVarSGD settings thetaSelectors costFxn g currentCache currentTheta = do

  (steppedThetas, steppedScore) <- stochasticStep settings thetaSelectors costFxn g currentTheta

  let 
    newCache = H.insert steppedThetas steppedScore currentCache
    (bestThetas, bestScore) = getMinScore newCache

    -- build the call to try again using updatedThetas, allowing us to explore worse directions, but every n step returning to best
    -- TODO use Reader monad
    callToContinueGD =  multiVarSGD settings thetaSelectors costFxn (snd $ next g) newCache

    -- converge if we try to descend and still find the same best thetas (within the goal threshold)
    converged = thetaDiff steppedThetas currentTheta <= (S.converganceGoal settings) || H.member steppedThetas currentCache

  -- TODO if we think we have converged, do one last pass with all threshold selectors to check all directions
  -- if that makes us better overall, continue with that, otherwise just finish
  -- this could improve accuracy, but will cost us in terms of time
  lastStepThetas <- return Nothing

  debugPrint $ "Score for this step is "++(show steppedScore)
  
  if not converged 
  then do
    debugPrint "\n" 
    callToContinueGD $ 
      case lastStepThetas of
        Nothing -> adjustForRestarts settings newCache steppedThetas bestThetas
        Just ts -> ts
  else do
    debugPrint ("\n\n\nFinished SGD with score = "++(show bestScore)
                  ++"\nUsing Theta: "++ (indent $ show bestThetas))
    return (bestThetas, bestScore, newCache)

{-
lastCheck = do 
    if converged
    then
      allDirThetas <- foldM (takeStep learnRate currentTheta costFxn) currentTheta thetaSelector
      if (thetaDiff allDirTheta bestThetas) <= goal
      then return Nothing
      else return $ Just allDirThetas
    else
      return Nothing
  -}

-- | Every time we hit a restartRound, jump to the current best
adjustForRestarts settings cache currThetas bestThetas = 
  if (H.size cache) `mod` (S.restartRound settings) == 0 
  then bestThetas 
  else currThetas

-- | in SGD, in each round, we randomly (g) select a few (batchSize) dimensions (thetaSelectors) to descend on
stochasticStep settings thetaSelectors costFxn g currentTheta = do
  -- TODO always take the thetas that were the most effective in the previous step
  let randSelectors = take (S.batchSize settings) $ shuffle' thetaSelectors (length thetaSelectors) g

  -- In GD, we must use a fixed theta (t) for updating each dimension (takeStep) of theta,
  -- and aggregate those these updates in a seperate copy of theta (updatesThetas) by using the fold
  debugPrint "Taking a step"
  steppedThetas  <- foldM (takeStep (S.learnRate settings) currentTheta costFxn) currentTheta randSelectors

  debugPrint "Scoring step"
  steppedScore <- costFxn steppedThetas

  return (steppedThetas, steppedScore)


getMinScore :: ResCache -> (Thetas, Double)
getMinScore cache = 
  H.foldlWithKey' (\(bestT,bestS) t s -> if bestS > s then (t,s) else (bestT,bestS)) (initThetas, read "Infinity") cache
  -- or, a more clear, but less efficent version
  -- minimumBy (comparing snd) $ H.toList cache 

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
  debugPrint ("Scoring program...\n"++(indent $ show newTheta))
  debugPrint ""
  return newTheta

-- | never move outside [-1,1] and
--   never update by more than [-0.2,0.2] in a single step
boundedUpdate learnRate slope prevVal  = let
  proposedStep = learnRate * slope
  boundedStep = max (-0.3) $ min (0.3) proposedStep
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

  debugPrint "Getting first score"
  -- score of current theta
  scoreOrig <- f t
  debugPrint $ "Calculating Partial Derivative wrt "++(thetaFieldChange t (over part (\x -> x+s) t))
  -- score of theta with small movement in dimension of interest
  scoreDelta <- f (over part (\x -> x+s) t)

  -- rise over run to find slope 
  let adjustment = (scoreOrig-scoreDelta)/s
  --debugPrint $ show s
  --debugPrint $ show scoreOrig
  --debugPrint $ show scoreDelta
  debugPrint $ "Derivative in "++(thetaFieldChange t (over part (\x -> x+s) t))++" = "++(show adjustment)
  return $ adjustment
