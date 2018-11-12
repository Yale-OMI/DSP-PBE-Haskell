module Settings.FixedSettings where

import Types.Thetas
import Data.Functor.Identity

debug = True


-- TODO Move this whole file to Settings.Options

-----------------
--
-- SGD Settings
--
-----------------

-- | selects the thetas should we vary during GD
thetaSelectors :: [(Double -> Identity Double) -> Thetas -> Identity Thetas]
thetaSelectors = [lpfThreshold, hpfThreshold,ringzFreq,ringzDecaySecs,ringzApp,lpfApp,hpfApp,whiteApp,ampApp,pitchShiftFreq,pitchShiftApp,idApp]

----------------
--
-- FFT settings
--
----------------

-- each bin is 1Hz and each frame is 1s
frameRes :: Int
frameRes = 4096

overlap :: Int
overlap = 2048

-- number of peaks to extract in spectral analysis/fingerprinting
numPeaks :: Int
numPeaks = 40 

-- what is the tolerance for considering two frequences to basically be the same
-- not in units of freq, but in how many peaks we allowed during constallation
binSize :: Int
binSize = 2

-- a higher value reduces the resolution of the fft
-- but can significantly improve running time
resolution :: Int
resolution = 0
