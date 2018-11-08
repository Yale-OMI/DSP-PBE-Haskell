{-# LANGUAGE OverloadedStrings #-}
module Analysis.FFT (auralDistance, peakList) where

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

import Analysis.FFT_Py

import Types.Common
import qualified Settings as S
import Utils

import Shelly (shelly, silently, run)
import Data.String

auralDistance :: (FilePath, AudioFormat) -> (FilePath, AudioFormat) -> IO Double
auralDistance a1 a2 = do
  ps1 <- peakList a1
  ps2 <- peakList a2
  -- since I am not looking at time shifting filter for now, I can zipWith over time to remove the time domain
  -- NB this means the times need to line up 'exactly', ie this will not work well for real-world examples
  --    but only for contrsucted examples where I apply the filter to the sample myself (eg in audacity)
  --mapM_ plot $ take 5 $ zip (map sort ps1) (map sort ps2) 
  let 
    distances = zipWith (comparePeaks) (map sort ps1) (map sort ps2)
    average ls = sum ls / genericLength ls
  return $ average distances 

plot :: ([Peak], [Peak]) -> IO()
plot (ps1, ps2) = do
  writeFile "tmp1.csv" (listToCSV ps1)
  writeFile "tmp2.csv" (listToCSV ps2)
  results <- shelly $ silently $ run (fromString "gnuplot") 
               [ "-p"
               , "plotter.gnuplot" ]
  return ()
  

comparePeaks ps1 ps2 = 
  sum $ zipWith comparePeak ps1 ps2

-- TODO rethink how i do distances...
-- | for a single time slice, do euclid distance treating (freq,amp) as (x,y)
--   should always have the same number of points, but they might not lineup exactly...
--   eg (3,0.1),(4,0.5),(5,0.5) vs (4,0.5),(5,0.5),(6,0.1) should be rated as fairly close
comparePeak :: Peak -> Peak -> Double
comparePeak peak1 peak2 = let
  freq1 = intToDouble $ getFreq peak1
  freq2 = intToDouble $ getFreq peak2
  amp1 = getAmp peak1
  amp2 = getAmp peak2
 in 
    {-if amp2 == 0
    then 1000 --penalize zero files
    else -}
    --traceShow ((show freq1)++" "++(show amp1)++", "++(show freq2)++" "++(show amp2) ) 
    (euclidDistance (freq1,amp1) (freq2,amp2)) / 1000-- + 1/(amp2+0.00001)

-- | break an audio file into time slices and i
--   find the freq peaks that are most predominate for each time slice
peakList :: (FilePath, AudioFormat) -> IO (OverTime (OverFreq Peak))
peakList (fp,a) = do
  peaks <- peakListPython fp
  let normPeaks = normalize peaks
  return $ getMainPeaks normPeaks

-- | Scale so max over all time slices is 1
--   note that this means some time slices will not have a max of 1
--   another option is to normlize so that the integral of the fft is 1
normalize :: [[Peak]] -> [[Peak]]
normalize ps = let
  maxPeak = maximum $ concatMap (map getAmp) ps
  scaleFactor = 1 / maxPeak 
 in
  map (map (\(frq,amp) -> (frq, amp* scaleFactor))) ps

{-
--takes wave file and turns it's values into list of Complex Doubles
wavList :: AudioFormat -> OverTime Double
wavList wav = let
    l1 = sampleData wav
  --takes at most a certain time frame
  in take (16384*10) $ elems $ amap (toSample) l1

-- mkFrames takes assocs of audio file and breaks it into 4096 sample (.09s) frames (overlapped by 50%)
mkFrames :: OverTime Double -> OverTime (OverTime Double)
mkFrames list1 =
  if (length list1) < S.frameRes --Settings.framerate
    then []
    else (take S.frameRes list1):(mkFrames (drop S.overlap list1))

-- | performs FFT on a list of samples, and conversts each sample to a list of peaks as the triple (freq,amp,phase)
--   returns a list (in the time domain) of peaks (freq domain) 
constellateAll :: OverTime (OverTime Double) -> OverTime (OverFreq Peak)
constellateAll timeSlices = let
    -- what the heck does this do?
    sparsifying n= foldr (.) id (replicate n (remove_every_nth 2))
    ars1 = --sparsifying S.resolution $ 
               --why not length sample here?
               map (\samples -> listArray (0,(S.frameRes - 1)) samples) timeSlices
    in map ((take (S.frameRes `div` 2)).assocs.constellate.rfft) ars1

-- constellate takes results of FFT and turns it into (amp,phase) at each frequency point
constellate :: Array Int (Complex Double) -> Array Int (Double,Double)
constellate arr1 =
  let list1 = assocs arr1
      polar1 = map (polar.snd) list1 --TODO use amap for array
      in listArray (0,((length list1) - 1)) polar1
-}

-- | with the full set of peaks for a list of frames,
--   we want to essentially apply a filter to each time slice
--   so that we only have the main peaks for that time slice
getMainPeaks :: OverTime (OverFreq Peak) -> OverTime (OverFreq Peak)
getMainPeaks ts =
  map (findBiggestPeaks. freqBins) ts

-- | partition a sample slice's peaks by freq into bins
--   this allows for a bit of flexibilty in freq analysis
--   ie dont need the exact same freqs to be the same
--   sameness of frequency is determined by bin size
--   NB the OverFreq interprtation seems to break down here
freqBins :: OverFreq Peak -> [[Peak]] 
freqBins = chunksOf S.binSize 

-- | for a given sample slices' peaks in each freq bin, 
--   get the biggest (by amp) in each bin
--   then take the biggest numPeaks of those
findBiggestPeaks :: [[Peak]] -> [Peak]
findBiggestPeaks ts = let
   -- first sort each bin, and only take the largest
   -- this eliminates the 'noisy' freqs nearby the peaks
   largestPeaksPerBin = map (last.(sortBy (comparing getAmp))) ts
   -- then we want to get the biggest peaks our of all the loudest bins (or 'freq areas')
   -- so we first sort by loudest 'freq areas'
   largestBinPerSampleSlice = reverse $ sortBy (comparing getAmp) largestPeaksPerBin
  in 
   -- to find the S.numPeaks biggest peaks
   take S.numPeaks $ largestBinPerSampleSlice

-- listTriple turns tuple of int and tuple and makes it into a triple
listTriple :: [(a,(b,b))] -> [(a,b,b)]
listTriple xs = 
   map (\(x,(y,z)) -> (x,y,z)) xs
