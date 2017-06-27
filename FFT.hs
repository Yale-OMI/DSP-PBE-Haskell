module FFT where

import Data.Complex
import Data.List
import Data.Ord
import HSoM.Examples.SpectrumAnalysis
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
comparePeaks :: [(Int,Double,Double)] -> [(Int,Double,Double)] -> [Double]
comparePeaks l1 l2 = let
    f x y = if (get1 x) == (get1 y)
      then abs(((get2 y) - (get2 x))/(get2 x))
      else 1000.000
    in zipWith f l1 l2
--takes wave file and turns it's values into list of Complex Doubles
wavList :: (Either String (Audio Int16)) -> [Double]
wavList wav = let
  l1 = assocs $ sampleData $ head $ rights[wav]
  in map (toSample.snd) l1
--performs FFT, converts to polar pairs and then to triples
constellateAll :: [[Double]] -> [[(Int,Double,Double)]]
constellateAll ls = let
    ars1 = map (\xs -> listArray (0,(S.frameRes - 1)) xs) ls
    in map (listTriple.assocs.constellate.rfft) ars1
--gets 5 peaks
getPeaks :: [[(Int,Double,Double)]] -> [[(Int,Double,Double)]]
getPeaks ts = let
  srts = map (reverse.(sortBy (comparing get2)).tail.(take (S.frameRes`div`2))) ts
  in map (take S.numPeaks) srts

getResults :: [[(Int,Double,Double)]] -> [[(Int,Double,Double)]] -> Double
getResults d1 d2 = let
   zips = zipWith (comparePeaks) d1 d2
   ds = concat zips
  in (sum ds)/(intToDouble(length ds))

peakResults :: (Either String (Audio Int16)) -> (Either String (Audio Int16)) -> Double
peakResults w1 w2 = let
  peak1 = getPeaks $ constellateAll $ mkFrames $ wavList w1
  peak2 = getPeaks $ constellateAll $ mkFrames $ wavList w2
  in getResults peak1 peak2
