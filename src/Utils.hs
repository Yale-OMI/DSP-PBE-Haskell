module Utils where


-- | Take the first instance from a list that satisfies the predicate
takeLast :: (a -> Bool) -> [a] -> Maybe a
takeLast p xs = case filter p xs of
  [] -> Nothing
  xs' -> Just $ last xs'

-- | Helper for printing
indent = ("\n" ++) . unlines. map ("   "++). lines

