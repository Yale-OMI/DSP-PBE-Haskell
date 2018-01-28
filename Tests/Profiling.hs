module Main where

import Synth
import Types.Common

import VividRunner
import Types.Filter

import System.Directory
import System.Environment
import GHC.IO.Encoding
import System.FilePath
import Codec.Wav

import Control.Monad

main = do
  let 
      inEx = "Sounds2/cartoon010.wav"
      outEx = "Sounds2/cartoon010-lpf800.wav"
      --inEx = "Sounds2/pinkFloydOrig.wav"
      --outEx = "Sounds2/pinkFloydFilt.wav"
  fileActions <- mapM importFile [inEx,outEx] :: IO [Either String (AudioFormat)]
  let testFilters = take 40 $map (\x-> (Thetas {_lpfThreshold=(x),_hpfThreshold=(-1),_ringzFreq=1,_ringzDecaySecs=1,_ringzApp=(-1),_lpfApp=(1),_hpfApp=(-1),_whiteApp=(-1),_ampApp=(1)})) [-(0.960),(-0.958)..]
  case sequence fileActions of
    Right fs -> do
      --rs <- mapM (\t -> testFilter inEx (head$ tail fs) $ thetaToFilter t) testFilters
      --printList $ zip [(-0.960),(-0.958)..] rs
      synthCode (inEx, head fs) (outEx, head $ tail fs) >>= print
    Left e -> error e
  return ()

printList :: (Show a,Show b) => [(a,b)] -> IO()
printList (l:ls) = do
  putStrLn ((show $ fst l)++","++(show $ snd l)) >> printList ls
