{-# LANGUAGE DataKinds #-}


-- Courtesy of Murphy :)

import Vivid

main = writeNRT "out.wav" $ do

   b <- newBufferFromFile "Sounds/A440.wav"

   play $ do
      -- The "::Float"s are not necessary but I'm in a rush (and works across GHC versions):
      s0 <- playBuf (buf_ b, loop_ (0::Float), doneAction_ (2::Float))
      lpf (freq_ (200::Float), in_ s0)

   -- This is janky and there's a solution:
   wait 5 -- length in secs of the sample
