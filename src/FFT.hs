module FFT where

-- need to create frequency frames for peaks after the FFT has been applied
import Data.Complex
import Data.List
import Data.Ord
--import HSoM.Examples.SpectrumAnalysis
import Codec.Wav
import Data.Audio
import Data.Either
import Data.Maybe
import Numeric.Extra
import Data.Int
import Data.Array.IArray
import Numeric.Transform.Fourier.FFT

import qualified Settings as S

-- constellate takes results of FFT and turns it into (amp,phase) at each frequency point
constellate :: Array Int (Complex Double) -> Array Int (Double,Double)
constellate arr1 =
  let list1 = assocs arr1
      polar1 = map (polar.snd) list1
      in listArray (0,((length list1) - 1)) polar1

-- mkFrames takes assocs of audio file and breaks it into 4096 sample (.09s) frames (overlapped by 50%)
mkFrames :: [Double] -> [[Double]]
mkFrames list1 =
  if (length list1) < S.frameRes --Settings.framerate
    then []
    else (take S.frameRes list1):(mkFrames (drop S.overlap list1))
-- listTriple turns tuple of int and tuple and makes it into a triple
listTriple :: [(a,(b,b))] -> [(a,b,b)]
listTriple xs = let
    a = (fst.head) xs
    b = (fst.snd.head) xs
    c = (snd.snd.head) xs
    in if null xs
      then []
      else (a,b,c):(listTriple (tail xs))
--gets 1st item of triple
get1 :: (a,b,c) -> a
get1 (a,_,_) = a
--gets 2nd item of triple
get2 :: (a,b,c) -> b
get2 (_,b,_) = b
--gets 3rd item of triple
get3 :: (a,b,c) -> c
get3 (_,_,c) = c
-- compares the lists of peaks in each frame and gets percent difference between them
comparePeaks :: (Int,Double,Double) -> (Int,Double,Double) -> Double
comparePeaks t1 t2 = let
  f1 = get1 t1
  f2 = get1 t2
  a1 = get2 t1
  a2 = get2 t2
  upstep = 2 ** (1/12)
  downstep = 2 ** (-1/12)
  in if (intToDouble f2) > ((intToDouble f1) * downstep) && (intToDouble f2) < ((intToDouble f1) * upstep)
    then (abs((a2 - a1)/a1))
    else 1000.000

--takes wave file and turns it's values into list of Complex Doubles
wavList :: (Either String (Audio Int16)) -> [Double]
wavList wav = let
  l1 = assocs $ sampleData $ head $ rights[wav]
  in map (toSample.snd) l1
--performs FFT, converts to polar pairs and then to triples
constellateAll :: [[Double]] -> [[(Int,Double,Double)]]
constellateAll ls = let
    ars1 = map (\xs -> listArray (0,(S.frameRes - 1)) xs) ls
    in map (listTriple.(take (S.frameRes `div` 2)).assocs.constellate.rfft) ars1
--gets 5 peaks
getPeaks :: [[(Int,Double,Double)]] -> [(Int,Double,Double)]
getPeaks ts = let
  freqs = map freqBins ts
  peaks = map findPeaks freqs
  in concat peaks

getResults :: [(Int,Double,Double)] -> [(Int,Double,Double)] -> Double
getResults d1 d2 = let
   zips = zipWith (comparePeaks) d1 d2
   in (sum zips)/(intToDouble(length zips))

peakResults :: (Either String (Audio Int16)) -> (Either String (Audio Int16)) -> Double
peakResults w1 w2 = let
  peak1 = getPeaks $ constellateAll $ mkFrames $ wavList w1
  peak2 = getPeaks $ constellateAll $ mkFrames $ wavList w2
  in getResults peak1 peak2

freqBins :: [(Int,Double,Double)] -> [[(Int,Double,Double)]]
freqBins ts =
  if (length ts) < S.binSize
  then []
  else (take S.binSize ts):(freqBins (drop S.binSize ts))

findPeaks :: [[(Int,Double,Double)]] -> [(Int,Double,Double)]
findPeaks ts = let
  srts = map (last.(sortBy (comparing get2))) ts
  in take 5 srts
