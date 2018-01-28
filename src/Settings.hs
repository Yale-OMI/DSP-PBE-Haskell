module Settings where


-- each bin is 1Hz and each frame is 1s
frameRes :: Int
frameRes = 4096

overlap :: Int
overlap = 2048

-- number of peaks to extract in spectral analysis/fingerprinting
numPeaks :: Int
numPeaks = 45

binSize :: Int
binSize = 2

restartRound :: Int
restartRound = 6
-- a higher value reduces the resolution of the fft
-- but can significantly improve running time
resolution :: Int
resolution = 0

debug :: String -> IO ()
debug = putStrLn
