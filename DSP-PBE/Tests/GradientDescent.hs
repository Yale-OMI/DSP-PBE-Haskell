{-# LANGUAGE RecordWildCards #-}

module Main where

import Synth.SGD
import Types.Thetas
import qualified Settings as S

import qualified Data.HashMap.Strict as H
import System.Random

----------
--
--  Test the stochastic gradient descent implementation and its components
--
----------

main :: IO ()
main = do
  
  rGen <- getStdGen
  _ <- multiVarSGD
          S.defaultOptions
          [ampApp]
          rGen
          1       --batchSize
          0.1     --convergance goal
          1       --learn rate
          initThetas
          simpleCost
          H.empty

  _ <- multiVarSGD
          S.defaultOptions
          [ampApp,lpfThreshold]
          rGen
          1       --batchSize
          0.1     --convergance goal
          1       --learn rate
          initThetas
          simpleCost2
          H.empty

  return ()

simpleCost :: Thetas -> IO Double
simpleCost Thetas{..} = 
  return $ abs _ampApp
   
simpleCost2 :: Thetas -> IO Double
simpleCost2 Thetas{..} = 
  return ((abs _ampApp) + (abs _lpfThreshold))
   
       
       
