{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Synth.Synth where

import Synth.SGD
import Synth.RTypeFilterGuess
import Synth.StructuralRefinement
--import Synth.RandInitGuess

import Analysis.FilterDistance

import Types.Common
import Types.Filter
import Types.Thetas

import qualified Settings as S
import Utils
import Control.Monad

import qualified Codec.Wav as W
import qualified Data.HashMap.Strict as H
import System.Random

-- | The main runner for DSP-PBE
--   this is a wrapper that handles the file io for runSynth
-- TODO have synthesis export a runnable filter
synthCode :: S.Options -> IO (Filter, Double)
synthCode settings@S.SynthesisOptions{..} = do
--  S.checkOptions settings
  fileActions <- mapM W.importFile [inputExample,outputExample] :: IO [Either String (AudioFormat)]
  (solutionProgram, score) <- case sequence fileActions of
    Right fs -> do
       runSynth
           settings
           (head fs)
           (head $ tail fs)
    Left e -> error e
  debugPrint $ show solutionProgram
  when (targetAudioPath /= "") $ runFilter resultantAudioPath targetAudioPath (toVivid solutionProgram) 10 >> return ()
  return (solutionProgram, score)

-- | Kicks off synthesis by using the user-provided refinements to select an initFilter
--   then enters the synthesis loop  
runSynth :: S.Options -> AudioFormat -> AudioFormat -> IO (Filter, Double)
runSynth settings@S.SynthesisOptions{..} in_audio out_audio = do
  initFilter <- guessInitFilter (inputExample,in_audio) (outputExample,out_audio)
  debugPrint "Starting with best filter as:"
  debugPrint $ show initFilter
  synthLoop settings out_audio H.empty H.empty initFilter

-- | If we scored below the user provided threshold, we are done
--   If not we dervive new constraints on synthesis and try again
synthLoop :: S.Options -> AudioFormat -> ThetaLog -> FilterLog -> Filter -> IO (Filter, Double)
synthLoop settings@S.SynthesisOptions{..} out_audio prevTLog prevFLog prevFilter = do
  debugPrint "Initiating strucutral synthesis..."
  let initFilter = if prevTLog == H.empty --special case to catch the first time through the loop
                   then prevFilter
                   else generateNewFilter settings prevFilter prevTLog prevFLog
  debugPrint "Found a program structure:"
  debugPrint $ show initFilter
  debugPrint "Initiating metrical synthesis..."
  (synthedThetas, score, tLog) <- parameterTuning settings inputExample (outputExample,out_audio) initFilter
  let synthedFilter = thetaOverFilter initFilter synthedThetas
  if score < epsilon
  then
    return (synthedFilter, score)
  else do
    let fLog = H.insert initFilter score prevFLog
    synthLoop settings out_audio tLog fLog synthedFilter

-- | Generate a new init filter based on the previous synthesis attempt
--   to derive structual constraints, find bad subpatterns from the log and avoid those
--   to apply dervived numerical constaints, just preserve the thetas of the previous filter
generateNewFilter :: S.Options -> Filter -> ThetaLog -> FilterLog -> Filter
generateNewFilter settings prevFilter tLog fLog = let
  -- find a new structure
  structure = structuralRefinement settings prevFilter tLog fLog
  -- use the prev log to find a good point for init thetas
  initThetas = filterToThetas $ fst $ getMinScore fLog
 in
  thetaOverFilter structure initThetas 



-- | This uses stochastic gradient descent to find a minimal cost theta
--   SGD is problematic since we cannot calculate a derivative of the cost function
--   TODO Another option might be to use http://www.jhuapl.edu/SPSA/ which does not require the derivative
--   TODO many options here https://www.reddit.com/r/MachineLearning/comments/2e8797/gradient_descent_without_a_derivative/
parameterTuning :: S.Options -> FilePath -> (FilePath, AudioFormat) -> Filter -> IO (Thetas, Double, ThetaLog)
parameterTuning settings in_audio_fp (out_fp, out_audio) initF = do
  let costFxn = testFilter in_audio_fp (out_fp, out_audio) . (thetaOverFilter initF)
  let rGen = mkStdGen 0
  debugPrint $ show (initF)
  debugPrint $ show (filterToThetas initF)
  solution <- multiVarSGD settings S.thetaSelectors costFxn rGen H.empty (filterToThetas initF)
  return solution

