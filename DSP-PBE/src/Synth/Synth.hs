{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Synth.Synth where

import System.Random

import Synth.SGD
import Synth.RTypeFilterGuess
import Synth.RandInitGuess

import Analysis.FilterDistance

import Types.Common
import Types.Filter

import qualified Data.HashMap.Strict as H

import qualified Settings as S
import Utils

import qualified Codec.Wav as W

-- | The main runner for DSP-PBE
-- TODO have synthesis export a runnable filter
synthCode :: S.Options -> IO ()
synthCode S.SynthesisOptions{..} = do
  fileActions <- mapM W.importFile [inputExample,outputExample] :: IO [Either String (AudioFormat)]
  solutionProgram <- case sequence fileActions of
    Right fs -> do
      solution <- 
        runSynth 
           (inputExample, head fs) 
           (outputExample, head $ tail fs)
      return solution
    Left e -> error e
  print solutionProgram
  runFilter resultantAudioPath targetAudioPath (toVivid solutionProgram) 10
  return ()

  
-- | generate the Vivid program to turn the in_example to the out_example
runSynth :: (FilePath, AudioFormat) -> (FilePath, AudioFormat) -> IO (Filter)
runSynth (in_filepath,in_audio) (out_filepath,out_audio) = do
  --First, determine a 'best guess' initFilter
  --TODO this might need to be a in a loop if we can learn a better after SGD
  --  myInitFilter <- guessInitFilter (in_filepath,in_audio) (out_filepath,out_audio)
  myInitFilter <- guessRandInitFilter (in_filepath,in_audio) (out_filepath,out_audio)
  debugPrint "Starting with best filter as:"
  debugPrint $ show myInitFilter
  --Once we have an initFilter, we refine it with SGD
  synthedFilter <- refineFilter in_filepath (out_filepath,out_audio) myInitFilter
  runFilter (S.tmpDir++S.finalWav) in_filepath (toVivid synthedFilter) 1.0
  return synthedFilter



-- | This uses stochastic gradient descent to find a minimal theta
--   SGD is problematic since we cannot calculate a derivative of the cost function
--   TODO Another option might be to use http://www.jhuapl.edu/SPSA/ which does not require the derivative
--   TODO many options here https://www.reddit.com/r/MachineLearning/comments/2e8797/gradient_descent_without_a_derivative/
optimize rGen tester initFilter = multiVarSGD
    S.thetaSelectors
    rGen
    4 --batch size (how many directions to test)
    0.01 --convergance goal
    1 --learn rate
    initFilter
    H.empty
    tester

-- | Adjust the params of a filter to get the best score
refineFilter :: FilePath -> (FilePath, AudioFormat) -> Thetas -> IO Filter
refineFilter in_audio_fp (out_fp, out_audio) initF = do
  let tester = testFilter in_audio_fp (out_fp, out_audio) . thetaToFilter
  rGen <- getStdGen
  solution <- optimize rGen tester initF
  return $ thetaToFilter $ fst solution

