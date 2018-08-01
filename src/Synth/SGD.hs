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
-- | calc new theta and build map of theta to value to avoid recomputation
multiVarSGD :: RandomGen g => _ -> g -> Int -> Double -> Double -> 
  Thetas -> Thetas -> ResCache -> (Thetas -> IO Double) -> IO (Thetas, ResCache)
multiVarSGD thetaSelectors g batchSize goal !learnRate !t bestTheta cache costFxn = do
  debugPrint ("Current cache: " ++ (show cache))
  -- NB use a fixed theta for takeStep, but update a seperate copy of theta in the fold
  (updatedThetas, newCache) <- foldM (takeStep learnRate t costFxn) (t,cache) (stochasticBatch g batchSize thetaSelectors)
    -- check if the updated choice is better (since we are working in highly non-convex space)
  (currentScore, newCache') <- runCostFxnWithCache newCache costFxn updatedThetas
  (prevBestScore, _)        <- runCostFxnWithCache newCache' costFxn bestTheta
  let 
    newBestTheta = if currentScore < prevBestScore 
                     then updatedThetas 
                     else bestTheta
    -- every now and then go back to the best we had found
    -- and decrease learn rate
    (newLearnRate,t') = if ((H.size cache)+1) `mod` (S.restartRound) == 0 
           then trace "BACKTRACKING SGD TO BEST SO FAR" (learnRate/2,newBestTheta)
           else (learnRate,updatedThetas)
    -- build the call to try again using updatedThetas, allowing us to explore worse directions, but every n step returning to best
    continueGD = multiVarSGD thetaSelectors (snd $ next g) batchSize goal newLearnRate t' newBestTheta newCache' costFxn
    -- converge if you try to descend and still find the same best thetas (within the goal threshold)
    converged = (thetaDiff updatedThetas bestTheta <= goal && (H.size cache >10)) || (H.size cache > 60)

  debugPrint $ "Current best candidate is"++(indent $ show $ thetaToFilter newBestTheta)
  debugPrint $ "Current score is "++(show currentScore)
  
  if not converged 
  then (trace "\n" continueGD)
  else do
    let (minThetaCache, minScoreCache) = getMinScore newCache
    return (trace ("\n\n\nFinished SGD with score = "++(show minScoreCache)
                      ++"\nUsing Theta: "++ (indent $ show $ thetaToFilter minThetaCache)) minThetaCache, newCache)

getMinScore :: ResCache -> (Thetas, Double)
getMinScore cache = 
  minimumBy (comparing snd) $ H.toList cache 


-- TODO make sure we always take the thetas that were the most effective in the previous step
stochasticBatch :: RandomGen g => g -> Int -> [a] -> [a]
stochasticBatch g batchSize xs =
  take batchSize $ shuffle' xs (length xs) g

takeStep :: Double -> Thetas -> (Thetas -> IO Double) -> (Thetas,ResCache) -> _ -> IO (Thetas,ResCache)
takeStep learnRate t f (updatedTheta,cache) part = do 
  (slope, newCache) <- partialDerivative f part t cache
  let newTheta = over part (\prevVal -> boundUpdate learnRate prevVal slope) updatedTheta --not allowed to move more than 0.2 in a single step
  debugPrint ("Adjusting "++(thetaFieldChange newTheta updatedTheta)++" by "++(show (thetaDiff updatedTheta newTheta)))
  debugPrint ("Scoring program...\n"++(indent $ show $ thetaToFilter newTheta))
  debugPrint ""
  return (newTheta, newCache)

boundUpdate learnRate prevVal slope =
  max (-1) $ min 1 $ (prevVal - (min 0.2 (learnRate * slope)))

-- TODO there must be a better way
-- but for now just move in one direction and interpolate 
partialDerivative :: (Thetas -> IO Double) -> _ -> Thetas -> ResCache -> IO (Double, ResCache)
partialDerivative f part t cache = do
  let s = 0.0001 --derivative step size
  --lookup/add to cache
  (score1, newCache') <- runCostFxnWithCache cache f t
  debugPrint $ "Calculating Partial Derivative wrt "++(thetaFieldChange t (over part (\x -> x+s) t))
  (score2, newCache) <- runCostFxnWithCache newCache' f (over part (\x -> x+s) t)
  let adjustment = ((score1-score2)/s)
  debugPrint $ "Derivative in "++(thetaFieldChange t (over part (\x -> x+s) t))++" = "++(show adjustment)
  return $ (adjustment, newCache)

runCostFxnWithCache :: ResCache -> (Thetas -> IO Double) -> Thetas -> IO (Double,ResCache)
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
  
