{-# LANGUAGE LambdaCase #-}

module Types.SCCode where

import Data.List
import GHC.Generics

import Types.Filter
import Types.PrettyFilter
import Types.DSPNode

rest :: [a] -> [a]
rest [] = []
rest (a:as) = as

data SCCode = 
      SCFilter { oid :: Int, vars :: [String], function :: String, args :: [(String, String)] }
    | SCWhiteNoise { oid :: Int, vars :: [String], iname :: String, function :: String, args :: [(String, String)] }
    | SCSeqCompose { oid :: Int, vars :: [String], iname :: String, f :: SCCode, f' :: SCCode }
    | SCParCompose { oid :: Int, vars :: [String], iname :: String, f :: SCCode, f' :: SCCode }

instance Show SCCode where
    show (SCFilter sc_id sc_vs fnc args) = 
        (sc_vs !! 0) ++ " = " ++ fnc ++ (getArgString args)  ++ ";\n"
    show (SCWhiteNoise sc_id sc_vs sc_in fnc args) = 
        (sc_vs !! 0) ++ " = Mix.ar([" ++ sc_in ++ ", " ++ fnc ++ (getArgString args) ++ "]);\n"  
    show (SCSeqCompose sc_id sc_vs sc_in f f') = 
        (show f) ++ (show f')
    show (SCParCompose sc_id sc_vs sc_in f f') = 
        (show f) ++ (show f') ++ (sc_vs !! 0) ++ 
        " = Mix.ar(2, [" ++ ((vars f) !! 0) ++ ", " ++ ((vars f') !! 0) ++ "]);\n" 
           
printSC :: SCCode -> String
printSC sc =
  "( \n" ++
  "SynthDef(\\dsp_pbe, {|out=0|\n" ++
  "var " ++ (intercalate "," ("main_in":(vars sc))) ++ ";\n" ++ 
  "main_in = " ++ playBuf ++ ";\n" ++ 
  (show sc) ++ 
  "Out.ar(out, " ++ ((vars sc) !! 0) ++ ");\n" ++  
  "}).add;\n" ++
  ")"
  where playBuf = "PlayBuf.ar(2, ~buf)"
 

makeSCFilter :: PrettyFilter -> Int -> String -> SCCode
makeSCFilter f ioid inn = let 
    scinfo  = getSCInfo f
    scfname = fst scinfo
    scargs  = snd scinfo
  in 
    case f of
      Node_p d -> makeSCFilterNode d ioid inn  scfname scargs
      (SequentialCompose f1 f2) -> SCSeqCompose { oid=(oid scf2), vars=new_vars, iname=inn, f=scf1, f'=scf2 }
              where scf1 = makeSCFilter f1 (ioid + 1) inn
                    scf2 = makeSCFilter f2 ((oid scf1) + 1) ((vars scf1) !! 0)
                    new_vars = (vars scf2) ++ (vars scf1)
      (ParallelCompose f1 f2) -> SCParCompose { oid=newOid, vars=new_vars, iname=inn, f=scf1, f'=scf2 }
              where scf1 = makeSCFilter f1 (ioid + 1) inn 
                    scf2 = makeSCFilter f2 ((oid scf1) + 1) inn
                    newOid = (oid scf2) + 1
                    new_vars = ("out" ++ (show newOid)):((vars scf2) ++ (vars scf1))
      (AmpApp_p a f)      -> SCSeqCompose { oid=(oid scf2), vars=new_vars, iname=inn, f=scf1, f'=scf2 }
              where scf1 = makeSCFilter f (ioid + 1) inn
                    scf2 = makeSCFilter (Node_p $ ID a) ((oid scf1) + 1) ((vars scf1) !! 0)
                    new_vars = (vars scf2) ++ (vars scf1)

makeSCFilterNode :: DSPNode -> Int -> String -> String -> [(String, String)] -> SCCode
makeSCFilterNode d ioid inn scfname scargs = case d of
  ID a            -> SCFilter { oid=ioid, vars=["id"++(show ioid)], function=scfname ++ inn, args=scargs}
  LPF t a         -> SCFilter { oid=ioid, vars=["lpf"++(show ioid)], function=scfname, args=("in", inn):scargs }
  HPF t a         -> SCFilter { oid=ioid, vars=["hpf"++(show ioid)], function=scfname, args=("in", inn):scargs } 
  PitchShift t a  -> SCFilter { oid=ioid, vars=["psh"++(show ioid)], function=scfname, args=("in", inn):scargs }
  Ringz t d a     -> SCFilter { oid=ioid, vars=["rgz"++(show ioid)], function=scfname, args=("in", inn):scargs }
  WhiteNoise x    -> SCWhiteNoise { oid=ioid, vars=["wns"++(show ioid)], iname=inn, function=scfname, args=scargs }

getSCInfo :: PrettyFilter -> (String, [(String, String)])
getSCInfo = \case
    Node_p d -> getSCInfoNode d
    SequentialCompose f f'  -> ("", [])
    ParallelCompose f f'    -> ("", [])
    AmpApp_p a f            -> ("", [])

getSCInfoNode :: DSPNode -> (String, [(String,String)])
getSCInfoNode = \case
    ID a                    -> ((show $ ampScale a) ++ " * ", [])  
    LPF t a                 -> ("LPF.ar", [("freq", show $ freqScale t), ("mul", show $ ampScale a)])
    HPF t a                 -> ("HPF.ar", [("freq", show $ freqScale t), ("mul", show $ ampScale a)])
    Ringz t d a             -> ("Ringz.ar", 
                                [
                                  ("freq", show $ freqScale t), 
                                  ("decaytime", show $ delayScale d), 
                                  ("mul", show $ ampScale a)
                                ])
    PitchShift t a          -> ("PitchShift.ar", [("pitchRatio", show $ freqScalePitchShift t), ("mul", show $ ampScale a)])
    WhiteNoise x            -> ("WhiteNoise.ar", [("mul", show $ ampScale x)]) 

getArgString :: [(String, String)] -> String
getArgString []   = ""
getArgString args = 
  let paired = map (\ x -> (fst x) ++ ": " ++ (snd x)) args
  in "(" ++ (foldr (\ x y -> x ++ ", " ++ y) (head paired) (rest paired)) ++ ")"

