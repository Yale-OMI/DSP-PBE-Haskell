module Utils where

import qualified Debug.Trace as D
import qualified Settings

traceMe x = D.traceShow x x
trace printVal returnVal= 
  if Settings.debug 
  then D.trace printVal returnVal
  else returnVal

debugPrint x =
  if Settings.debug
  then print x
  else return ()

-- | Take the first instance from a list that satisfies the predicate
takeLast :: (a -> Bool) -> [a] -> Maybe a
takeLast p xs = case filter p xs of
  [] -> Nothing
  xs' -> Just $ last xs'

-- | Helper for printing
indent = ("\n" ++) . unlines. map ("   "++). lines

remove_every_nth :: Int -> [a] -> [a]
remove_every_nth = recur 1
    where recur _ _ []     = []
          recur i n (x:xs) = if i == n
            then recur 1 n xs
            else x:recur (i+1) n xs
  
euclidDistance (x1, y1) (x2, y2) = sqrt (x'*x' + y'*y')
    where
      x' = x1 - x2
      y' = y1 - y2
