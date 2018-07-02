module Settings where


debug = True

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

--How often to we go back to the best Theta we found so far
restartRound :: Int
restartRound = 5

-- a higher value reduces the resolution of the fft
-- but can significantly improve running time
resolution :: Int
resolution = 0

tmpDir :: String
tmpDir = "tmp2/"

finalWav :: String
finalWav = "final.wav" 
