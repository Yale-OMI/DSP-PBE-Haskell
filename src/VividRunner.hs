{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module VividRunner where
-- Courtesy of Murphy :)

import Vivid

import Control.Monad

--runFilter :: FilePath -> (Signal -> _) -> IO(String)
runFilter srcFile vCode = do
   writeNRT out_filepath $ vAction srcFile vCode
   return out_filepath
 where
   out_filepath = "tmp/out.wav"

vAction srcFile vCode = do
   b <- newBufferFromFile srcFile

   play $ do
     -- The "::Float"s are not necessary but I'm in a rush (and works across GHC versions):
     --playBuf (buf_ b, loop_ (0::Float), doneAction_ (2::Float))
     s0 <- playBuf (buf_ b, loop_ (0::Float), doneAction_ (2::Float))
     vCode s0

   -- This is janky and there's a solution:
   wait 2 -- length in secs of the sample


