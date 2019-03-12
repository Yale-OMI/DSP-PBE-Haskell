module Main where

import Synth.Synth
import Analysis.FilterDistance
import Types.Common
import Types.Filter
import Types.PrettyFilter
import Types.DSPNode
import Synth.RTypeFilterGuess

import System.Directory
import System.FilePath
import System.Environment
import GHC.IO.Encoding
import System.FilePath
import Codec.Wav

import Data.List
import Data.Ord

import Control.Monad

import TestUtils

exDir = "Sounds/SynthesisBenchmarks/Constructed/"
cartoonEx = exDir ++ "cartoon010.wav"
cartoonEx800 = exDir ++ "cartoon010-lpf800.wav" 
cartoonEx5000 = exDir ++ "cartoon010-lpf5000.wav" 
cartoonEx1500 = exDir ++ "cartoon010-hpf1500.wav" 
btsEx = exDir ++ "BTS-DNA-short.wav" 
btsEx2000 = exDir ++ "BTS-DNA-short-lpf2000.wav" 
filenames  =  
  [ cartoonEx
  , cartoonEx800
  , cartoonEx5000
  , btsEx
  , btsEx2000
  , cartoonEx1500
  ]

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8  

  audios' <- mapM getFile filenames
  let audios = zip filenames audios'
  let audioExPairs = [
          (audios!!0,audios!!1)
        , (audios!!0,audios!!2)
        , (audios!!3,audios!!4)
        ]

  refinementTypeTest audioExPairs
  plotCostFxnTest

-- Plot rtype costs over lpf examples
refinementTypeTest audios = do
  mapM (uncurry guessInitFilter) audios >>= (mapM print)
  return ()

-- Plot freq cutoff vs cost for lpf examples
-- seems to be a memory leak in here somewhere - dont try to run all three at the same time
plotCostFxnTest = do
  --getCostMap cartoonEx cartoonEx800 (take 120 [(invFreqScale 8000),(invFreqScale 8000)+0.01..]) >>= printList
  --getCostMap cartoonEx cartoonEx5000 (take 60 [(invFreqScale 1000),(invFreqScale 1000)+0.01..]) >>= printList
  getCostMap cartoonEx cartoonEx1500 (take 60 [(invFreqScale 100),(invFreqScale 100)+0.01..]) >>= printList
  --getCostMap btsEx btsEx2000 (take 80 [(invFreqScale 1),(invFreqScale 1)+0.01..]) >>= printList

getCostMap :: FilePath -> FilePath -> [Double] -> IO [(Int,Double)]
getCostMap inEx outEx range = do
  print ("Distances for :" ++ inEx ++ " & " ++ outEx)
  fileActions <- mapM importFile [inEx,outEx] :: IO [Either String (AudioFormat)]
  let testFilters = 
        map 
          (\x-> toInternalFilter $ Node_p $ HPF x 1)
          range
  case sequence fileActions of
    Right fs -> do
      rs <- mapM (\t -> testFilter inEx (outEx, head$ tail fs) t) testFilters
      return $ zip (map (floor. freqScale) range) rs
    Left e -> error e

printList :: (Ord a, Ord b, Show a,Show b) => [(a,b)] -> IO()
printList xs = do
  putStrLn $ "Minimum cost found :" ++ (show $ minimumBy (comparing snd) xs)
  mapM_ (\x -> putStrLn ((show $ fst x)++", "++(show $ snd x))) xs
