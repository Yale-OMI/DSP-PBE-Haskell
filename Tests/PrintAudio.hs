module PrintAudio where

import System.FilePath
import Data.Audio
import Codec.Wav
import Data.Int

import Types.Common
import Data.Array.IArray

printAudio fPath = do
  file <- importFile fPath :: IO (Either String (AudioFormat))
  case file of
    Right f -> printAsMatlab $ wavList f 
    Left e -> error e

printAsMatlab :: [(Double,Float)] -> IO()
printAsMatlab as =
  mapM_ (\(t,s)-> putStrLn ((show s)++" "++(show t)++"")) as

wavList :: Audio Int16 -> [(Double,Float)]
wavList wav = let
    l1 = sampleData wav
  in zip (take (16384*10) $ elems $ amap toSample l1) [(1/44100),(2/44100)..]
