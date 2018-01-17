{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}


module SGD where

import Debug.Trace

import System.IO.Unsafe

import System.Random.Shuffle
import System.Random

import qualified Data.HashMap.Strict as H
import Data.Hashable
import GHC.Generics
import Control.Lens
import Control.Monad

data Thetas = Thetas{
   _lpfThreshold :: Double,
   _hpfThreshold :: Double
   } deriving (Eq,Show,Generic,Hashable)

makeLenses ''Thetas

type ResCache = H.HashMap Thetas Double

-- | Use data type Theta for params to be passed to eval fxn
-- | calc new theta and build map of theta to value to avoid recomputation
multiVarSGD :: _ -> Int -> Double -> Double -> Thetas -> Thetas -> ResCache -> (Thetas -> IO Double) -> (Thetas, ResCache)
multiVarSGD thetaSelectors batchSize goal learnRate t t_prev cache f =
  let
    --TODO better convergance fxn
    converged = (abs((_lpfThreshold t)-(_lpfThreshold t_prev)) <= goal)
    -- NB use a fixed theta for takeStep, but update a seperate copy of theta in the fold
    (updatedThetas, newCache) = foldr (takeStep learnRate t f) (t,cache) (stochasticBatch batchSize thetaSelectors)

    continueGD = multiVarSGD thetaSelectors batchSize goal learnRate updatedThetas t newCache f
  in
    if not converged
    then continueGD
    else (trace "finished SGD" t, newCache)

stochasticBatch :: Int -> [a] -> [a]
stochasticBatch batchSize xs =
  take batchSize $ shuffle' xs (length xs) rand
 where
  --TODO get rid of unsafe here, pass in the gen from the top io level and there is no problem
  rand = unsafePerformIO newStdGen


-- TODO get rid of unsafe here? (actually its fine here, but would be a nice exercise to fix)
takeStep :: Double -> Thetas -> (Thetas -> IO Double) -> _ -> (Thetas,ResCache) -> (Thetas,ResCache)
takeStep learnRate t f part (updatedTheta,cache) = let
  (deriveCalc, newCache) = unsafePerformIO $ partialDerivative f part t cache
 in
  (over part (\x -> max 0 $ x - learnRate * deriveCalc) updatedTheta, newCache)

-- TODO there must be a better way
-- but for now just move in one direction and interpolate 
partialDerivative :: (Thetas -> IO Double) -> _ -> Thetas -> ResCache -> IO (Double, ResCache)
partialDerivative f part t cache = do
  --lookup in cache
  (x1, newCache) <- case H.lookup t cache of
       Just v -> return (v,cache)
       Nothing -> f t >>= (\v -> return (v, H.insert t v cache))
  print x1
  -- x2 is not likely to ever be calculated again, so dont bother saving it in the newCache
  x2 <- f (over part (\x -> x*1.01) t)
  print x2
  return $ (((x2 - x1) / 0.01),newCache)

