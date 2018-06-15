module Analysis.FFT (peakResults, peakList) where

-- need to create frequency frames for peaks after the FFT has been applied
import Data.Complex
import Data.List
import Data.List.Split
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

peakResults :: AudioFormat -> AudioFormat -> Double
peakResults w1 w2 =
  getResults (peakList w1) (peakList w2)
 
peakList :: AudioFormat -> [Peak]
peakList = 
  getPeaks. constellateAll. mkFrames. wavList

getResults :: [Peak] -> [Peak] -> Double
getResults d1 d2 = let
   distances = zipWith (comparePeak) d1 d2
   average ls = sum ls / genericLength ls
   in average distances 

-- compare a peak to get distance on xy plane based on freq and amp
-- can also try other metrics here
comparePeak :: Peak -> Peak -> Double
comparePeak t1 t2 = let
  freq1 = intToDouble $ getFreq t1
  freq2 = intToDouble $ getFreq t2
  amp1 = getAmp t1
  amp2 = getAmp t2
  upstep = 2 ** (1/12)
  downstep = 2 ** (-1/12)
  distance (x1, y1) (x2, y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2
  in 
    {-if amp2 == 0
    then 1000 --penalize zero files
    else -}
    --traceShow ((show freq1)++" "++(show amp1)++", "++(show freq2)++" "++(show amp2) ) 
    distance (freq1,amp1) (freq2,amp2) -- + 1/(amp2+0.00001)


remove_every_nth :: Int -> [a] -> [a]
remove_every_nth = recur 1
    where recur _ _ []     = []
          recur i n (x:xs) = if i == n
            then recur 1 n xs
            else x:recur (i+1) n xs

-- | performs FFT on a list of samples, and conversts each sample to a list of peaks as the triple (freq,amp,phase)
constellateAll :: [[Double]] -> [[Peak]]
constellateAll ls = let
    sparsifying n= foldr (.) id (replicate n (remove_every_nth 2))
    ars1 = sparsifying S.resolution $ map (\xs -> listArray (0,(S.frameRes - 1)) xs) ls
    in map (listTriple.(take (S.frameRes `div` 2)).assocs.constellate.rfft) ars1

-- constellate takes results of FFT and turns it into (amp,phase) at each frequency point
constellate :: Array Int (Complex Double) -> Array Int (Double,Double)
constellate arr1 =
  let list1 = assocs arr1
      polar1 = map (polar.snd) list1 --TODO use amap for array
      in listArray (0,((length list1) - 1)) polar1

-- mkFrames takes assocs of audio file and breaks it into 4096 sample (.09s) frames (overlapped by 50%)
mkFrames :: [Double] -> [[Double]]
mkFrames list1 =
  if (length list1) < S.frameRes --Settings.framerate
    then []
    else (take S.frameRes list1):(mkFrames (drop S.overlap list1))

--takes wave file and turns it's values into list of Complex Doubles
wavList :: AudioFormat -> [Double]
wavList wav = let
    l1 = sampleData wav
  in take (16384*10) $ elems $ amap toSample l1

-- | with the peaks for a list of frames, chose
getPeaks :: [[Peak]] -> [Peak]
getPeaks ts = let
  freqBins :: [Peak] -> [[Peak]]
  freqBins = chunksOf S.binSize -- partition a sample's peaks by freq
 in
  concatMap (findPeaks. freqBins) ts

-- | for a given sample's peaks in each freq bin, 
--   get the biggest (by amp) in each bin
--   then take the biggest numPeaks of those
findPeaks :: [[Peak]] -> [Peak]
findPeaks ts = let
   largestPeaksPerBin = map (last.(sortBy (comparing getAmp))) ts
--  in take S.numPeaks $ reverse $ sortBy (comparing getAmp) largestPeaksPerBin
  in take S.numPeaks  largestPeaksPerBin

traceMe x = traceShow x x



-- listTriple turns tuple of int and tuple and makes it into a triple
listTriple :: [(a,(b,b))] -> [(a,b,b)]
listTriple xs = 
   map (\(x,(y,z)) -> (x,y,z)) xs

