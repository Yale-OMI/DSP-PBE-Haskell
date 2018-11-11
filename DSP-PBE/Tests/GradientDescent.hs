{-# LANGUAGE RecordWildCards #-}

module Main where

import Synth.SGDNoCache
import Types.Thetas

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
          [ampApp]
          rGen
          1       --batchSize
          0.1     --convergance goal
          1       --learn rate
          initThetas
          simpleCost
  return ()

simpleCost :: Thetas -> IO Double
simpleCost Thetas{..} = 
  return $ abs _ampApp
   
       
