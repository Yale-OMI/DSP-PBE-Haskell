module Utils where

import qualified Debug.Trace as D
import qualified Settings
import Types.Common

import qualified Data.HashMap.Strict as H 
import qualified Data.Map as M
import Data.List

traceMe x = D.traceShow x x
trace printVal returnVal= 
  if Settings.debug 
  then D.trace printVal returnVal
  else returnVal

debugPrint x =
  if Settings.debug
  then putStrLn x
  else return ()

listToCSV :: [Peak] -> String
listToCSV = 
  concatMap (\(f,a) -> (show f) ++ ", " ++ (show a) ++ "\n")

-- | return the key val pair with the minimal val
--   this is in contrast to M.findMin, which returns the minmal key
findMinByVal :: Ord v => M.Map k v -> (k,v)
findMinByVal m =
  minimumBy c $ M.toList m
 where
  c x y = case snd x < snd y of
    True -> LT
    _    -> GT

-- | Take the first instance from a list that satisfies the predicate
takeLast :: (a -> Bool) -> [a] -> Maybe a
takeLast p xs = case filter (not. p) xs of
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
