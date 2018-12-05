{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}

module Types.Filter where

import Vivid

import qualified Data.HashMap.Strict as H
import Data.Hashable

import Data.List

import GHC.Generics
import Control.Lens

import Text.Printf

import Debug.Trace

-- TODO seperate structure and filter into two datatypes
data Filter = 
    ID Float
  | HPF Float Float
  | LPF Float Float
  | PitchShift Float Float
  | Ringz Float Float Float
  | WhiteNoise Float
  | ParallelCompose Filter Filter
  | SequentialCompose Filter Filter
  | AmpApp Float Filter
  deriving (Eq, Generic, Hashable)

-- A log of the best filter for each structure
-- TODO should be StructureLog = H.HashMap Structure Double
type FilterLog = H.HashMap Filter Double

-- | show with *** for parallel composition, and >>> for sequential composition
--   these are close, but not exactly the same meanings as Haskell's Control.Arrow combinators
instance Show Filter where
  show (ParallelCompose f f') = (show f) ++ (" *** ") ++ (show f')
  show (SequentialCompose f f') = (show f) ++ (" >>> ") ++ (show f')
  show (AmpApp x f) = "SetVolume: " ++ (showAmp x) ++ "% >>> " ++ show f
  show (WhiteNoise x) = "WhiteNoise: " ++ (showAmp x) ++ "% "
  show (ID a) = "Identity: "++ (showAmp a)
  show (HPF fq a) = "HiPass: "++ (showFreq fq) ++ " " ++ (showAmp a)
  show (LPF fq a) = "LoPass: "++ (showFreq fq) ++ " " ++ (showAmp a)
  show (PitchShift fq a) = "PitchShift: "++ (showFreqP fq) ++ " " ++ (showAmp a)
  show (Ringz fq d a) = "Ringz: "++ (showFreq fq) ++ " " ++ (showDelay d) ++ " " ++ (showAmp a)

freqScale x = ((x+1)*10000)+100  -- freq operations 100<x<16k Hz
freqScalePitchShift x = x*2000  -- pitchshift -2000<x<2000 Hz
invFreqScale x = ((x-100)/10000)-1
ampScale x = (x+1)/2.2           -- amp operations 0<x<.9 so there is always space to explore 'up' for derivative calcuation
delayScale x = (x+1)/10             -- delay operations 0<x<.2

showAmp amp = "amp@"++(printf "%.2f" $ ampScale amp)
showFreq freq = "freq@" ++ (printf "%.0f" $ freqScale freq)
showFreqP freq = "freq@" ++ (printf "%.0f" $ freqScalePitchShift freq)
showDelay d = "delay@" ++ (printf "%.2f" $ delayScale d)

--implements feature scaling so during GD our thetas are -1<t<1
--we onyl scale them back to the appropriate values when we need to apply theatas in a filter
toVivid :: Filter -> SDBody' '[] Signal -> SDBody' '[] Signal
toVivid = \case
      ID a                   -> (\bufs -> (ampScale a::Float) ~* bufs)
      HPF t a                -> (\bufs -> (ampScale a::Float) ~* hpf (freq_ (freqScale t::Float), in_ bufs))
      LPF t a                -> (\bufs -> (ampScale a::Float) ~* lpf (freq_ (freqScale t::Float), in_ bufs))
      PitchShift t a         -> (\bufs -> (ampScale a::Float) ~* freqShift (freq_ (freqScalePitchShift t::Float), in_ bufs)) -- there is also pitchShift in vivid, but it is more complex
      WhiteNoise a           -> (\bufs -> (ampScale a::Float) ~* whiteNoise)
      Ringz f d a            -> (\bufs -> (ampScale a::Float) ~* ringz (freq_ (freqScale f::Float), decaySecs_ (delayScale d::Float), in_ bufs))
      ParallelCompose f f'   -> (\bufs -> do (((toVivid f) bufs) ~+ ((toVivid f') bufs)))
      SequentialCompose f f' -> (\bufs -> (toVivid f') $ (toVivid f) bufs )
      AmpApp a f             -> (\bufs -> do (((a+2)/3) ~* ((toVivid f) bufs))) -- can only turn down the total vol down to 1/3

supercolliderPrint :: Filter -> String
supercolliderPrint f = toSCWrap $ toSC f

-- create variable declarations
-- create playbuf and out signal lines
toSCWrap :: ([String], (String, String)) -> String
toSCWrap scf =
  "( \n" ++
  "SynthDef(\\dsp_pbe, {|out=0|\n" ++
  "var " ++ (intercalate "," (fst scf)) ++ ";\n" ++ 
  (fst (snd scf)) ++ playBuf ++ (snd (snd scf)) ++ 
  "Out.ar(out, " ++ ((fst scf) !! 0) ++ ");\n" ++  
  "}).add;\n" ++
  ")"
  where playBuf = "PlayBuf.ar(2, filename)"

-- returns a tuple with the first element being an array of
-- var names and the second being another tuple of two strings
-- var names must be collected so they may be defined late (toSCWrap
-- takes care of this)
-- TODO: Abstract SuperColliderFunction printing, 
-- some function that could take an array of pairs and output the SC
-- function call with the parameters formatted correctly
toSC :: Filter -> ([String], (String, String))
toSC = \case
  LPF t a  -> ([varName], (varName ++ " = LPF.ar(in:", ", freq: " ++ (show t) ++ 
                                                       ", mul: " ++ (show a) ++ ");\n"))
                      where varName = "lpfS" -- ++ (show t) ++ (show a)
  HPF t a  -> ([varName], (varName ++ " = RHPF.ar(in: ", ", freq: " ++ (show t) ++ 
                                                         ", mul: " ++ (show a) ++ ");\n"))
                      where varName = "hpfS" -- ++ (show t) ++ (show a)
  Ringz fq d a -> ([varName], (varName ++ " = Ringz.ar(in: ", ", freq: " ++ (show fq) ++ 
                                                              ", decaytime: " ++ (show d) ++ 
                                                              ", mul: " ++ (show a)++ ");\n")) 
                      where varName = "ringzS" -- ++ (show d) ++ (show a)
  PitchShift t a -> ([varName], (varName ++ " = PitchShift.ar(in: ", ", pitchRatio: " ++ (show t) ++ 
                                                                     ", mul: " ++ (show a) ++ ");\n"))
                      where varName = "pitchShiftS"
  WhiteNoise x -> ([varName], (varName ++ " = Mix.ar([", ", " ++ "WhiteNoise.ar(" ++ (show x) ++ "]);\n"))
                      where varName = "whiteNoiseS"
  SequentialCompose f1 f2 -> (vars, (fst (snd scf1), nscf2))
                      where
                        scf1 = toSC f1
                        scf2 = toSC f2
                        nscf2 = (snd (snd scf1)) ++
                                " \n " ++  
                                (fst (snd scf2)) ++ 
                                ((fst scf1) !! 0) ++
                                (snd (snd scf2))
                        vars = (fst scf2) ++ (fst scf1)

  ParallelCompose f1 f2 -> (vars, (nscf1, nscf2))
                      where 
                        scf1 = toSC f1 
                        scf2 = toSC f2
                        varName = ((fst scf2) !! 0) ++ "P" ++ ((fst scf1) !! 0)
                        varNameOut = "out_" ++ varName
                        varNameIn = "in_" ++ varName
                        vars  = varNameOut:varNameIn:((fst scf2) ++ (fst scf1))
                        nscf1 = varNameIn ++ " = "
                        nscf2 = ";\n" ++
                                (fst (snd scf1)) ++ 
                                varNameIn ++
                                (snd (snd scf1)) ++
                                " \n " ++
                                (fst (snd scf2)) ++
                                varNameIn ++ 
                                (snd (snd scf2)) ++
                                " \n " ++
                                varNameOut ++ 
                                " = "++
                                ((fst scf1) !! 0) ++ " + " ++
                                ((fst scf2) !! 0) ++ ";\n"

