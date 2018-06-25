module Settings where


debug = True

-- each bin is 1Hz and each frame is 1s
frameRes :: Int
frameRes = 4096

overlap :: Int
overlap = 2048

-- number of peaks to extract in spectral analysis/fingerprinting
numPeaks :: Int
numPeaks = 45

-- what is the tolerance for considering two frequences to basically be the same
-- not in units of freq, but in how many peaks we allowed during constallation
binSize :: Int
binSize = 20

restartRound :: Int
restartRound = 6
-- a higher value reduces the resolution of the fft
-- but can significantly improve running time
resolution :: Int
resolution = 0

tmpDir :: String
tmpDir = "tmp2/"

finalWav :: String
finalWav = "final.wav" 
