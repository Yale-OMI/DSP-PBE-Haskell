{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE BangPatterns #-}

module Synth.SGD where

import System.Random.Shuffle
import System.Random

import qualified Data.Map as M
import Data.List
import Data.Ord

import GHC.Generics
import Control.Lens
import Control.Monad

import Types.Filter
import qualified Settings as S

import Utils

-- | Use data type Theta for params to be passed to eval fxn
--   calc new theta and build map of theta to value to avoid recomputation
multiVarSGD :: RandomGen g => 
  S.Options ->
  (Filter -> IO Double) ->
  g ->                                        -- ^ a random generator for the Stochastic-ness of SGD
  FilterLog ->
  [Filter -> (Double -> Double) -> Filter] -> -- ^ updaters for all dimension of theta
  Filter ->                                   -- ^ the current theta, from which we will descend the gradient
  IO (Filter, Double, FilterLog)               -- ^ returns the solution, the cost, and the log of all attempts
multiVarSGD settings costFxn g currentCache thetaSelectors currentTheta = do

  (steppedThetas, steppedScore) <- stochasticStep settings thetaSelectors costFxn g currentTheta

  let
    newCache = M.insert steppedThetas steppedScore currentCache
    (bestThetas, bestScore) = findMinByVal newCache
    
    -- build the call to try again using updatedThetas, allowing us to explore worse directions, but every n step returning to best
    -- TODO use Reader monad
    callToContinueGD =  multiVarSGD settings costFxn (snd $ next g) newCache

    -- converge if we try to descend and still find the same best thetas (within the goal threshold)
    converged = filterDiff steppedThetas currentTheta <= (S.converganceGoal settings) || M.member steppedThetas currentCache

  -- TODO if we think we have converged, do one last pass with all threshold selectors to check all directions
  -- if that makes us better overall, continue with that, otherwise just finish
  -- this could improve accuracy, but will cost us in terms of time
  debugPrint $ "Score for this step is "++(show steppedScore)
  debugPrint $ "FOUND IN CACHE:   " ++ (show $ M.member steppedThetas currentCache)

  if converged || 
     isNaN steppedScore || 
     M.size newCache > (S.thetaLogSizeTimeout settings)
  then do
    when converged $ debugPrint "------ Metrical synthesis converged"
    when (isNaN steppedScore) $ debugPrint "------ NAN for score"
    when (M.size newCache > (S.thetaLogSizeTimeout settings)) $ debugPrint "------ log size timeout"
    debugPrint ("\n[+] Finished SGD with score = "++(show bestScore)
                  ++"\nUsing Theta: "++ (indent $ show bestThetas))
    return (bestThetas, bestScore, newCache)
  else do
    let 
      newTheta =
        adjustForRestarts settings newCache steppedThetas bestThetas
      newThetaUpdaters = [head $ extractThetaUpdaters newTheta]
    print $ "---------- "++(show $ M.size newCache)
    callToContinueGD newThetaUpdaters newTheta 

{-
lastCheck = do 
    if converged
    then
      allDirThetas <- foldM (takeStep learnRate currentTheta costFxn) currentTheta thetaSelector
      if (filterDiff allDirTheta bestThetas) <= goal
      then return Nothing
      else return $ Just allDirThetas
    else
      return Nothing
  -}

-- | Every time we hit a restartRound, jump to the current best
adjustForRestarts settings cache currThetas bestThetas = 
  if (M.size cache) `mod` (S.restartRound settings) == 0 
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



-- | descend by a single step in the direction of the largest gradient over a single dimension
takeStep :: 
     Double  -- ^ the learning rate
  -> Filter 
  -> (Filter -> IO Double) 
  -> Filter
  -> (Filter -> (Double -> Double) -> Filter) 
  -> IO Filter
takeStep learnRate t f updatedTheta part = do 
  slope <- partialDerivative f part t
  let newTheta = part updatedTheta (boundedUpdate learnRate slope) --not allowed to move more than 0.2 in a single step
  debugPrint ("Adjusting "++(filterFieldChange newTheta updatedTheta)++" by "++(show (filterDiff updatedTheta newTheta)))
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
     (Filter -> IO Double)                    -- ^ distance metric
  -> (Filter -> (Double -> Double) -> Filter) -- ^ dimension updater
  -> Filter                                   -- ^ current theta to take partial derivative of
  -> IO Double                                -- ^ derivative and updated cache
partialDerivative f part t = do
  -- derivative approximation step size TODO, move to settings?
  let s = 0.01 

  debugPrint "Getting first score"
  -- score of current theta
  scoreOrig <- f t

  debugPrint("F1: " ++ (drawFilter t) ++ "\nF2: " ++ (drawFilter (part t (\x -> x+s))))
  debugPrint $ "Calculating Partial Derivative wrt "++(filterFieldChange t (part t (\x -> x+s) ))
  -- score of theta with small movement in dimension of interest
  scoreDelta <- f (part t (\x -> x+s))

  -- rise over run to find slope 
  let adjustment = (scoreOrig-scoreDelta)/s
  --debugPrint $ show s
  --debugPrint $ show scoreOrig
  --debugPrint $ show scoreDelta
  debugPrint $ "Derivative in "++(filterFieldChange t (part t (\x -> x+s)))++" = "++(show adjustment)
  --TODO should end synthesis at this point by taking whatever we had as the best so far
  -- when (isNaN adjustment) $ error "Generated NaN. This probably means that all filters have been zero'ed out and FFT doesnt know what to do"
  return adjustment
