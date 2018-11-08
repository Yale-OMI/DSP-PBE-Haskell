module TestUtils where

import Types.Common
import Types.Filter
import Codec.Wav


getFile :: FilePath -> IO(AudioFormat)
getFile filepath = do
  let f x = either (error "failed") id x
  w <- importFile $ filepath :: IO(Either String (AudioFormat))
  return $ f w 

makePairs :: [a] -> [(a,a)]
makePairs (x:y:ys) = (x,y): makePairs ys
makePairs [x] = []
makePairs [] = []
