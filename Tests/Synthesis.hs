module Main where

import Synth.Synth
import Analysis.FilterDistance
import Types.Common
import Types.Filter

import System.Directory
import System.Environment
import GHC.IO.Encoding
import System.FilePath
import Codec.Wav

import Control.Monad

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8  

  let 
      inEx = "Sounds/SynthesisBenchmarks/Constructed/cartoon010.wav"
      outEx = "Sounds/SynthesisBenchmarks/Constructed/cartoon010-lpf800.wav" 
  fileActions <- mapM importFile [inEx,outEx] :: IO [Either String (AudioFormat)]
  let testFilters = take 30 $ map (\x-> (Thetas {_lpfThreshold=(x),_hpfThreshold=(-1),_ringzFreq=1,_ringzDecaySecs=1,_ringzApp=(-1),_lpfApp=(1),_hpfApp=(-1),_whiteApp=(-1),_ampApp=(1)})) [-(0.99),(-0.98)..]
  case sequence fileActions of
    Right fs -> do
      rs <- mapM (\t -> testFilter inEx (outEx, head$ tail fs) $ thetaToFilter t) testFilters
      printList $ zip (map (floor. freqScale) [(-0.99),(-0.98)..]) rs
      --synthCode (inEx, head fs) (outEx, head $ tail fs) >>= print
    Left e -> error e
  return ()

printList :: (Show a,Show b) => [(a,b)] -> IO()
printList ([]) = return ()
printList (l:ls) = do
  putStrLn ((show $ fst l)++","++(show $ snd l)) >> printList ls
