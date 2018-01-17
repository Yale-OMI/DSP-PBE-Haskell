{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module SGD (multiVarSGD) where

import Debug.Trace

import System.Process
import System.IO.Unsafe

import System.Random.Shuffle
import System.Random

import Control.Lens

data Thetas = Thetas{
   _lpfThreshold :: Double,
   _hpfThreshold :: Double,
  } deriving (Show)

makeLenses ''Thetas


-- | Use data type Theta for params to be passed to driver
multiVarSGD :: _ -> Int -> Double -> Double -> Thetas -> Thetas -> (Thetas -> IO Double) -> Thetas
multiVarSGD thetaSelectors batchSize goal learnRate t t_prev f =
  let
    --TODO better convergance fxn
    converged = (abs((_speed t)-(_speed t_prev)) <= goal)
    continueGD = multiVarSGD thetaSelectors batchSize goal learnRate updatedThetas t f
    -- NB use a fixed theta for takeStep, but update a seperate copy of theta in the fold
    updatedThetas = foldr (takeStep learnRate t f) t (stochasticBatch batchSize thetaSelectors)
  in
    if not converged
    then continueGD
    else trace "finished SGD" t

stochasticBatch :: Int -> [a] -> [a]
stochasticBatch batchSize xs =
  take batchSize $ shuffle' xs (length xs) rand
 where
  --TODO get rid of unsafe here, pass in the gen from the top io level and there is no problem
  rand = unsafePerformIO newStdGen


-- TODO get rid of unsafe here? (actually its fine here, but would be a nice exercise to fix)
takeStep :: Double -> Thetas -> (Thetas -> IO Double) -> _Lens -> Thetas -> Thetas
takeStep learnRate t f part updatedTheta = 
  over part (\x -> x - learnRate * (unsafePerformIO $ partialDerivative f part t)) updatedTheta

-- TODO there must be a better way
-- but for now just move in one direction and interpolate 
partialDerivative :: (Thetas -> IO Double) -> _Lens -> Thetas -> IO Double
partialDerivative f part t = do
  x1 <- f t
  x2 <- f (over part (\x -> x*1.01) t)
  return $ ((x2 - x1) / 0.01)

