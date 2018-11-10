module Main where

import Synth.SGD

----------
--
--  Test the stochastic gradient descent implementation and its components
--
----------

main :: IO ()
main = do
  
  rGen <- getStdGen
  multiVarSGD
        rGen
        1       --batchSize
        0.1     --convergance goal
        1       --learn rate
        initFilter
        H.empty
        simpleCost

simpleCost :: Thetas -> IO Double
simpleCost ts = 
  return 
   
        
 
