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
  (_,c1,_) <- multiVarSGD
          S.defaultOptions
          [ampApp]
          simpleCost
          rGen
          H.empty
          initThetas

  (_,c2,_) <- multiVarSGD
          S.defaultOptions
          [ampApp,lpfThreshold]
          simpleCost2
          rGen
          H.empty
          initThetas

  if c1 < 0.2
  then putStrLn "PASSED GD in 1 dimension" >> return ()
  else error "FAILED GD in 1 dimension"

  if c2 < 0.2
  then putStrLn "PASSED GD in 2 dimensions" >> return ()
  else error "FAILED GD in 2 dimensions"

  return ()

simpleCost :: Thetas -> IO Double
simpleCost Thetas{..} = 
  return $ abs _ampApp
   
simpleCost2 :: Thetas -> IO Double
simpleCost2 Thetas{..} = 
  return ((abs _ampApp) + (abs _lpfThreshold))
   
       
       
