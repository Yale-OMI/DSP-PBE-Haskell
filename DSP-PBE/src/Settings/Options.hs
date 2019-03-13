{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}

module Settings.Options where
import System.Console.CmdArgs

import Control.Monad
import System.Exit
import System.Directory

----------
--
--  These are settings that the user passes in to the tool
--
----------
    
data Options 
  = SynthesisOptions {
    inputExample :: FilePath
  , outputExample :: FilePath
  , targetAudioPath :: FilePath
  , resultantAudioPath :: FilePath
  , epsilon :: Double
  , smartStructuralRefinement :: Bool
  , filterLogSizeTimeout :: Int
  , thetaLogSizeTimeout :: Int

  -- SGD specific
  , batchSize :: Int
  , learnRate :: Double
  , converganceGoal :: Double
  , restartRound :: Int -- ^ How often to we go back to the best Theta we found so far
  , verbose :: Bool
  } deriving (Show, Data, Typeable)

defaultEpsilon = 1
defaultFilterLogSizeTimeout = 3
defaultThetaLogSizeTimeout = 30

defaultRestartRound = 5
defaultBatchSize = 4
defaultLearnRate = 0.1
defaultConverganceGoal = 0.01

defaultOptions = SynthesisOptions {
    inputExample = "" &= help "The input audio example file" &= typDir
  , outputExample = "" &= help "The output audio example file" &= typDir
  , targetAudioPath = "" &= help "The target audio example path on which to apply the generated transformation" &= typDir
  , resultantAudioPath = "" &= help "The path on which to save the generated transformation" &= typDir
  , epsilon = defaultEpsilon &= help ("The thershold for aural distance at which point we can say we succeded in synthesis: "++(show defaultEpsilon)) &= (typ "DOUBLE")
  , smartStructuralRefinement = True &= help "When turned off, uses brute force exploration of structural space. Turned on by default" &= (typ "BOOL")
  , filterLogSizeTimeout = defaultFilterLogSizeTimeout &= help ("max number of structures to try before timing out - setting it very low for now: "++(show defaultFilterLogSizeTimeout)) &= (typ "INT")
  , thetaLogSizeTimeout = defaultThetaLogSizeTimeout &= help ("max number of thetas to try before timing out of SGD and triggering strucutral synthesis: "++(show defaultFilterLogSizeTimeout)) &= (typ "INT")
  , batchSize = defaultBatchSize &= help ("Batch size for SGD. Default ="++(show defaultBatchSize)) &= (typ "INT")
  , learnRate = defaultLearnRate &= help ("Learn rate for SGD. Too small, and we will think we have converged, too large and we will \'bounce\' around. Default ="++(show defaultLearnRate)) &= (typ "Double")
  , converganceGoal = defaultConverganceGoal &= help ("what do we condsider to be a negligible gain from one step of SGD. Set higher to converge sooner. Default ="++(show defaultConverganceGoal)) &= (typ "DOUBLE")
  , restartRound = defaultRestartRound &= help ("How often should we restart GD at the last best found theta. Default ="++(show defaultRestartRound)) &= (typ "INT")
  , verbose = False
  }

checkOptions SynthesisOptions{..} = do
  when (inputExample == "")  $ die "SynthSynth: Must provide input audio example. See help with --help"
  when (outputExample == "") $ die "SynthSynth: Must provide output audio example. See help with --help"
  inputExists <- doesFileExist inputExample
  outputExists <- doesFileExist outputExample
  when (not $ inputExists) $ die ("SynthSynth: Could not find input audio example : " ++ inputExample ++ ". See help.")
  when (not $ outputExists) $ die "SynthSynth: Could not find output audio example provided. See help."

