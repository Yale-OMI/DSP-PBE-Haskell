module VividWork where
{-# LANGUAGE DataKinds #-}
import Vivid

test :: SynthDef
test = sd (0 ::I "note") $ do
    s <- 0.1 ~* sinOsc (freq_ $ midiCPS (V::V "note"))
     out 0 [s, s]
