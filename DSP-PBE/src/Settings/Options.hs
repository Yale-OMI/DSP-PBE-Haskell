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
  , verbose :: Bool
  } deriving (Show, Data, Typeable)

defaultOptions = SynthesisOptions {
    inputExample = "" &= help "The input audio example file" &= typDir
  , outputExample = "" &= help "The output audio example file" &= typDir
  , targetAudioPath = "" &= help "The target audio example path on which to apply the generated transformation" &= typDir
  , resultantAudioPath = "" &= help "The path on which to save the generated transformation" &= typDir
  , verbose = False
  }

checkSettings SynthesisOptions{..} = do
  inputExists <- doesFileExist inputExample
  outputExists <- doesFileExist outputExample
  when (inputExists) $ die "SynthSynth: Could not find input audio example. See help."
  when (outputExists) $ die "SynthSynth: Could not find output audio example provided. See help."

