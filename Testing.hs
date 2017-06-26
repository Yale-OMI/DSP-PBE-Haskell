module Testing where
import FFT
import Data.Audio
import Codec.Wav
import Data.Int
main :: IO ()
main = do
  w1 <- importFile "PianoC.wav" :: IO(Either String (Audio Int16))
  w2 <- importFile "PianoSoft.wav" :: IO(Either String (Audio Int16))
  w3 <- importFile "PianoFilter.wav" :: IO(Either String (Audio Int16))
  w4 <- importFile "HornC.wav" :: IO(Either String (Audio Int16))
  let test1v2 = peakResults w1 w2
  let test1v3 = peakResults w1 w3
  let test1v4 = peakResults w1 w4
  let test2v3 = peakResults w2 w3
  let test2v4 = peakResults w2 w4
  let test3v4 = peakResults w3 w4
  return () 
