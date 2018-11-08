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
  ResCache -> -- ^ the cache of cost calculation for all the thetas we have seen so far
  (Thetas -> IO Double) -> 
  IO (Thetas, ResCache)
multiVarSGD thetaSelectors g batchSize goal !learnRate !currentTheta cache costFxn = do
  debugPrint ("Current cache: " ++ (show $ map (\(t,s) -> (thetaToFilter t,s))$ H.toList cache))

  -- in SGD, in each round, we randomly (g) select a few (batchSize) dimensions (thetaSelectors) to descend on
  let randSelectors = stochasticBatch g batchSize thetaSelectors
  -- In GD, we must use a fixed theta (t) for updating each dimension (takeStep) of theta,
  -- and aggregate those these updates in a seperate copy of theta (updatesThetas) by using the fold
  (steppedThetas, newCache) <- foldM (takeStep learnRate currentTheta costFxn) (currentTheta, cache) randSelectors

  -- If we have a perfectly convex space, updatedThetas would be the end of this round of SGD
  -- However, since the distance calculation in not perfectly convex, we need to check if the updated choice is really better 
  (steppedScore, newCache') <- runCostFxnWithCache newCache costFxn steppedThetas

  let 
    (bestTheta, bestScore) = getMinScore newCache'

    -- every now and then go back to the best we had found and decrease learn rate
    -- if we missed something we might have too large a learn rate and are hopping over it
    (newLearnRate,t') = 
       if ((H.size cache)+1) `mod` (S.restartRound) == 0 
       then trace "BACKTRACKING SGD TO BEST SO FAR" (learnRate/2, bestTheta)
       else (learnRate, steppedThetas)

    -- build the call to try again using updatedThetas, allowing us to explore worse directions, but every n step returning to best
    continueGD = multiVarSGD thetaSelectors (snd $ next g) batchSize goal newLearnRate t' newCache' costFxn
    -- converge if we try to descend and still find the same best thetas (within the goal threshold)
    converged = (thetaDiff steppedThetas bestTheta <= goal && (H.size cache >10)) || (H.size cache > 60)

  debugPrint $ "Current best candidate is"++(indent $ show $ thetaToFilter bestTheta)
  debugPrint $ "Score for best candidate is "++(show bestScore)
  debugPrint $ "Score for this step is "++(show steppedScore)
  
  if not converged 
  then (trace "\n" continueGD)
  else do
    debugPrint ("\n\n\nFinished SGD with score = "++(show bestScore)
                  ++"\nUsing Theta: "++ (indent $ show $ thetaToFilter bestTheta))
    return (bestTheta, newCache')

getMinScore :: ResCache -> (Thetas, Double)
getMinScore cache = 
  H.foldlWithKey' (\(bestT,bestS) t s -> if bestS > s then (t,s) else (bestT,bestS)) (initFilter, read "Infinity") cache
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
  -> (Thetas,ResCache) 
  -> _ 
  -> IO (Thetas,ResCache)
takeStep learnRate t f (updatedTheta,cache) part = do 
  (slope, newCache) <- partialDerivative f part t cache
  let newTheta = over part (boundedUpdate learnRate slope) updatedTheta --not allowed to move more than 0.2 in a single step
  debugPrint ("Adjusting "++(thetaFieldChange newTheta updatedTheta)++" by "++(show (thetaDiff updatedTheta newTheta)))
  debugPrint ("Scoring program...\n"++(indent $ show $ thetaToFilter newTheta))
  debugPrint ""
  return (newTheta, newCache)

-- | never move outside [-1,1] and
--   never update by more than [-0.2,0.2] in a single step
boundedUpdate learnRate slope prevVal  = let
  proposedStep = learnRate * slope
  boundedStep = max (-0.2) $ min (0.2) proposedStep
 in  
  -- (-) here because if the tangent has positive slope (increasing prevVal increases cost), we need to go in the opposite direction
  max (-1) $ min 1 $ (prevVal - boundedStep) 

-- | Calculate the derivative of one dimension of the theta
--   since we cannot calculate the derivative of the cost function, we just approximate the deritative with a small step
--   this is an inefficent method (althought the caching helps) - other options are listed in Synth/Synth.hs
partialDerivative  :: 
     (Thetas -> IO Double) -- ^ distance metric
  -> _                     -- ^ dimension selector
  -> Thetas                -- ^ current theta to take partial derivative of
  -> ResCache              -- ^ current cache
  -> IO (Double, ResCache) -- ^ derivative and updated cache
partialDerivative f part t cache = do
  -- derivative approximation step size TODO, move to settings?
  let s = 0.01 

  -- score of current theta
  (score1, newCache') <- runCostFxnWithCache cache f t
  debugPrint $ "Calculating Partial Derivative wrt "++(thetaFieldChange t (over part (\x -> x+s) t))
  -- score of theta with small movement in dimension of interest
  (score2, newCache) <- runCostFxnWithCache newCache' f (over part (\x -> x+s) t)

  -- rise over run to find slope 
  let adjustment = ((score1-score2)/s)*0.001
  debugPrint $ "Derivative in "++(thetaFieldChange t (over part (\x -> x+s) t))++" = "++(show adjustment)
  return $ (adjustment, newCache)

-- | Get the cost of a candidate theta by first checking the cache for that value and 
--   on a miss calculating and saving the result in the cache
runCostFxnWithCache :: 
      ResCache             -- ^ current cache
  -> (Thetas -> IO Double) -- ^ distance metric (has the original target output audio baked in)
  -> Thetas                -- ^ candidate theta
  -> IO (Double,ResCache)  -- ^ score of candidate and an updated cache
runCostFxnWithCache cache f t = 
  case H.lookup t cache of
    Just v -> do
      debugPrint "Found theta in cache"
      return (v,cache)
    Nothing -> do  
      debugPrint "Did not find this Theta in cache, calculating score and adding to cache..."
      score <- f t 
      debugPrint $ "This Theta scored: " ++ (show score)
      return (score, H.insert t score cache)
  
