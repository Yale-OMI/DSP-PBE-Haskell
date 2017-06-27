module Untitled where
import FFT
import Codec.Wav
import Data.Audio
import Data.Int

main :: IO()
main = do
  wav1 <- importFile "PianoC.wav" :: IO(Either String (Audio Int16))
  wav2 <- importFile "HornC.wav"  :: IO(Either String (Audio Int16))
  wav3 <- importFile "PianoCSharp.wav" :: IO(Either String (Audio Int16))
  let a1 = head $ getPeaks $ constellateAll $ mkFrames $ wavList wav1
  let a2 = head $ getPeaks $ constellateAll $ mkFrames $ wavList wav2
  let a3 = head $ getPeaks $ constellateAll $ mkFrames $ wavList wav3
  print a1
  print a2
  print a3
  return ()
