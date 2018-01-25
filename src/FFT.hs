module FFT (peakResults) where

-- need to create frequency frames for peaks after the FFT has been applied
import Data.Complex
import Data.List
import Data.Ord
import Data.Audio
import Numeric.Extra
import Data.Int
import Data.Array.IArray
import Numeric.Transform.Fourier.FFT

import Control.Parallel.Strategies

import Types.Common
import qualified Settings as S

import Debug.Trace 

peakResults :: Audio Int16 -> Audio Int16 -> Double
peakResults w1 w2 = let
  mapTup f (x,y) = withStrategy (parTuple2 rdeepseq rdeepseq) (f x,f y)
  (peak1, peak2) = mapTup (getPeaks. constellateAll. mkFrames. wavList) (w1,w2)
  in getResults peak1 peak2

getResults :: [Peak] -> [Peak] -> Double
getResults d1 d2 = let
   distances = zipWith (comparePeak) d1 d2
   average ls = sum ls / genericLength ls
   in average distances 

-- compare a peak to get distance on xy plane based on freq and amp
-- can also try other metrics here
comparePeak :: Peak -> Peak -> Double
comparePeak t1 t2 = let
  freq1 = intToDouble $ get1 t1
  freq2 = intToDouble $ get1 t2
  amp1 = get2 t1
  amp2 = get2 t2
  upstep = 2 ** (1/12)
  downstep = 2 ** (-1/12)
  distance (x1, y1) (x2, y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2
  in distance (freq1,amp1) (freq2,amp2)
    {-
    if 
      (intToDouble f2) > ((intToDouble f1) * downstep) && 
      (intToDouble f2) < ((intToDouble f1) * upstep)
    then (abs((a2 - a1)/a1))
    else traceShow t1 1000.000-}


--gets 5 peaks
getPeaks :: [[Peak]] -> [Peak]
getPeaks ts = concatMap (findPeaks. freqBins) ts

findPeaks :: [[Peak]] -> [Peak]
findPeaks ts = let
  srts = map (last.(sortBy (comparing get2))) ts
  in take 5 srts

remove_every_nth :: Int -> [a] -> [a]
remove_every_nth = recur 1
    where recur _ _ []     = []
          recur i n (x:xs) = if i == n
            then recur 1 n xs
            else x:recur (i+1) n xs

--performs FFT, converts to polar pairs and then to triples
constellateAll :: [[Double]] -> [[Peak]]
constellateAll ls = let
    sparsifying n= foldr (.) id (replicate n (remove_every_nth 2))
    ars1 = sparsifying S.resolution $ map (\xs -> listArray (0,(S.frameRes - 1)) xs) ls
    in map (listTriple.(take (S.frameRes `div` 2)).assocs.constellate.rfft) ars1

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

freqBins :: [Peak] -> [[Peak]]
freqBins ts =
  if (length ts) < S.binSize
  then []
  else (take S.binSize ts):(freqBins (drop S.binSize ts))

--takes wave file and turns it's values into list of Complex Doubles
wavList :: Audio Int16 -> [Double]
wavList wav = let
  l1 = assocs $ sampleData wav
  in map (toSample.snd) l1


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
