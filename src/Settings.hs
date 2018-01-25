module Settings where


-- each bin is 1Hz and each frame is 1s
frameRes :: Int
frameRes = 44100

overlap :: Int
overlap = 22050

numPeaks :: Int
numPeaks = 6

binSize :: Int
binSize = 200

-- a higher value reduces the resolution of the fft
-- but can significantly improve running time
resolution :: Int
resolution = 4
