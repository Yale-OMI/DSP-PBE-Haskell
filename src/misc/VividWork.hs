{-# LANGUAGE DataKinds #-}

import Vivid
theSound = sd (60 ::I "note") $ do
    wobble <- sinOsc (freq_ 5) ? KR ~* 10 ~+ 10
    s <- 1 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
    out 0 [s,s]

loPass x = sd (60 ::I "note") $ do
  wobble <- sinOsc (freq_ 5) ? KR ~* 10 ~+ 10
  s <- 1 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ wobble)
  lo <- lpf (in_ s, freq_ x)
  out 0 [lo,lo]

main :: IO()
main = do
  theSound <- synth theSound ()
  wait 2.5
  free theSound
  wait 2.5
  --let synthes = map [100..200] loPass
  --mapM synthes makeBuffer
  loPass1 <- synth (loPass 200) ()
  wait 2.5
  loPass2 <- synth (loPass 60) ()
  wait 2.5
  free loPass1
  free loPass2
  return()
