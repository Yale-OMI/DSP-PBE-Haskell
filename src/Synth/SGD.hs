{-# LANGUAGE PartialTypeSignatures #-}


module Synth.SGD where

import System.Random.Shuffle
import System.Random

import qualified Data.HashMap.Strict as H
import GHC.Generics
import Control.Lens
import Control.Monad

import Types.Filter
import qualified Settings as S

import Utils

-- | Use data type Theta for params to be passed to eval fxn
-- | calc new theta and build map of theta to value to avoid recomputation
multiVarSGD :: RandomGen g => _ -> g -> Int -> Double -> Double -> Thetas -> Thetas -> ResCache -> (Thetas -> IO Double) -> IO (Thetas, ResCache)
multiVarSGD thetaSelectors g batchSize goal learnRate t t_best cache f = do
  -- NB use a fixed theta for takeStep, but update a seperate copy of theta in the fold
  (updatedThetas, newCache) <- foldM (takeStep learnRate t f) (t,cache) (stochasticBatch g batchSize thetaSelectors)
    -- converge if you try to descend and still do the best
  let converged = thetaDiff updatedThetas t_best <= goal
    -- check if the updated choice is better (since we are working in highly non-convex space)
  (thisVal, newCache') <- runIO newCache f updatedThetas
  (oldBest,_)          <- runIO cache f t_best
  let 
    newBest = if thisVal<oldBest then updatedThetas else t_best
    --every now and then go back to the best we had found
    t' = if (H.size cache) `mod` (S.restartRound) == 0 then newBest else updatedThetas
    -- build the call to try again, allowing us to explore worse directions, but every n step returning to best
    continueGD = multiVarSGD thetaSelectors (snd $ next g) batchSize goal learnRate t' newBest newCache' f

  debugPrint $ "Current best candidate is"++(indent $ show $ thetaToFilter updatedThetas)
  debugPrint $ "Current score is "++(show thisVal)
  
  if not converged 
  then (trace "\n" continueGD)
  else return (trace ("finished SGD with score = "++(show thisVal)) t, newCache)

-- TODO make sure we always take the thetas that were the most effective in the previous step
stochasticBatch :: RandomGen g => g -> Int -> [a] -> [a]
stochasticBatch g batchSize xs =
  take batchSize $ shuffle' xs (length xs) g

takeStep :: Double -> Thetas -> (Thetas -> IO Double) -> (Thetas,ResCache) -> _ -> IO (Thetas,ResCache)
takeStep learnRate t f (updatedTheta,cache) part = do 
  (deriveCalc, newCache) <- partialDerivative f part t cache
  let newTheta = over part (\x -> x - (min 0.2 (learnRate * deriveCalc))) updatedTheta --not allowed to move more than 0.2 in a single step
  let boundedNewTheta = over part (\x -> max (-1) $ min 1 $ x) newTheta
  debugPrint ("Adjusting "++(thetaFieldChange newTheta updatedTheta)++" by "++(show (thetaDiff updatedTheta boundedNewTheta)))
  debugPrint ("Scoring program...\n"++(indent $ show $ thetaToFilter boundedNewTheta))
  debugPrint ""
  return (boundedNewTheta, newCache)


-- TODO there must be a better way
-- but for now just move in one direction and interpolate 
partialDerivative :: (Thetas -> IO Double) -> _ -> Thetas -> ResCache -> IO (Double, ResCache)
partialDerivative f part t cache = do
  let s = 0.001 --derivative step size
  --lookup in cache
  (x1, newCache) <- runIO cache f t
  debugPrint $ "Calculating Partial Derivative wrt "++(thetaFieldChange t (over part (\x -> x+s) t))
  -- x2 is not likely to ever be calculated again, so dont bother saving it in the newCache
  x2 <- f (over part (\x -> x+s) t)
  debugPrint $ "Derivative in "++(thetaFieldChange t (over part (\x -> x+s) t))++" = "++(show $ (x1-x2)/s)
  return $ (((x2 - x1) / s),newCache)

runIO :: ResCache -> (Thetas -> IO Double) -> Thetas -> IO (Double,ResCache)
runIO cache f t = do 
  (x, newCache) <- case H.lookup t cache of
    Just v -> return (v,cache)
    Nothing -> f t >>= (\v -> return (v, H.insert t v cache))
  return (x,newCache)
  
