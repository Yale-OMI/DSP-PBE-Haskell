{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Analysis.FilterDistance (
    runFilter,
    testFilter )
  where

import Codec.Wav

import Analysis.FFT
import Types.Common
import Types.Filter
import Utils
import Vivid
import System.FilePath

-- | Tells us the distance between the current filter results and the target audio data
testFilter :: FilePath -> (FilePath, AudioFormat) -> Filter -> IO AuralDistance
testFilter in_fp (out_fp,outAudio) f = do
  let vividCode = toVivid f
  newOutFilepath <- runFilter "tmp2/out.wav" in_fp vividCode 1.0
  newAudio <- importFile newOutFilepath :: IO(Either String AudioFormat)
  case newAudio of
    Left e -> error ("Hit an error: "++e)
    --Right a -> return $ auralDistance outAudio a
    Right a -> auralDistance (out_fp,outAudio) (newOutFilepath,a)

-- | A small wrapper to geneate the vAction, perform that action in nonrealtime (writeNRT), 
--   then save it in out_filepath
runFilter :: FilePath -> FilePath -> (SDBody' '[] Signal -> SDBody' '[] Signal) -> Float -> IO(String)
runFilter out_filepath srcFile vCode secsToGenerate = do
   writeNRTWith (defaultNRTArgs {_nrtArgs_sampleRate = 44100}) out_filepath $ vAction secsToGenerate srcFile vCode 
   return out_filepath

-- | This is the template of the Vivid (SuperCollider) function that we use 
--   to construct a runnable Vivid program from a filter (vCode) given a particular audio input (srcFile)
vAction :: Float -> FilePath -> (SDBody' '[] Signal -> SDBody' '[] Signal) -> _
vAction secsToGenerate srcFile vCode = do
   b <- newBufferFromFile srcFile

   play $ do
     -- The "::Float"s are not necessary but I'm in a rush (and works across GHC versions):
     --playBuf (buf_ b, loop_ (0::Float), doneAction_ (2::Float))
     s0 <- playBuf (buf_ b, loop_ (0::Float), doneAction_ (2::Float))
     vCode $ toSig s0

   -- This is janky and there's a solution:
   wait secsToGenerate -- length in secs of the sample
   closeBuffer b

