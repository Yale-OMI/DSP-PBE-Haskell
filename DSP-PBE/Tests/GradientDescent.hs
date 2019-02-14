{-# LANGUAGE RecordWildCards #-}

module Main where

import Synth.SGD
import qualified Settings as S

import Types.Filter
import Types.PrettyFilter
import Types.DSPNode
import Data.Tree

import qualified Data.Map.Strict as M
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
          simpleCost
          rGen
          M.empty
          ([head $ extractThetaUpdaters initFilter])
          initFilter
  
  (_,c2,_) <- multiVarSGD
          S.defaultOptions
          simpleCost2
          rGen
          M.empty
          (extractThetaUpdaters initFilter)
          initFilter
  
  if c1 < 0.2
  then putStrLn "PASSED GD in 1 dimension" >> return ()
  else error "FAILED GD in 1 dimension"

  if c2 < 0.2
  then putStrLn "PASSED GD in 2 dimensions" >> return ()
  else error "FAILED GD in 2 dimensions"

  return ()

initFilter = toInternalFilter $ LPF_p 1 1

simpleCost :: Filter -> IO Double
simpleCost f = 
  return $ abs $ snd $ head $ getParams $ nodeContent $ rootLabel f
   
simpleCost2 :: Filter -> IO Double
simpleCost2 f = 
  return $ abs $ sum $ map snd $ getParams $ nodeContent $ rootLabel f
       
       
