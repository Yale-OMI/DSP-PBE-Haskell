module Main where

import Synth.Synth
import Analysis.FilterDistance
import Types.Common
import Types.Filter

import System.Directory
import System.Environment
import GHC.IO.Encoding
import System.FilePath
import Codec.Wav

import Control.Monad

main = do
  setLocaleEncoding utf8
  setFileSystemEncoding utf8
  setForeignEncoding utf8  
  args <- getArgs
  -- a workaround now to transform new audio, pass the target file in on the command line
  -- TODO have synthesis export a runnable filter
  when (length args /= 3) $ error "Usage: musicSynth <in file> <out file> <audio to transform>"

  let 
      inEx = head args
      outEx = head $ tail args 
      audioToTransform = head $ tail $ tail args
  fileActions <- mapM importFile [inEx,outEx] :: IO [Either String (AudioFormat)]
  case sequence fileActions of
    Right fs -> do
      solution <- synthCode (inEx, head fs) (outEx, head $ tail fs)
      print solution --TODO indent here
      runFilter "tmp2/transformed.wav" audioToTransform (toVivid solution) 10
    Left e -> error e
  return ()

printList :: (Show a,Show b) => [(a,b)] -> IO()
printList ([]) = return ()
printList (l:ls) = do
  putStrLn ((show $ fst l)++","++(show $ snd l)) >> printList ls
