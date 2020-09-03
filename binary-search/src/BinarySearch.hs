module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x
    | null arr      = Nothing
    | arr ! m == x  = Just m
    | arr ! m > x   = find (ixmap (l, m-1) id arr) x
    | otherwise     = find (ixmap (m+1, r) id arr) x
    where
        (l, r) = bounds arr
        m      = l + (r-l) `div` 2
{-
-- alternative ("iterative") solution
find arr x
  | null arr = Nothing
  | otherwise = let result = foldr (\step lo -> if (lo + step <= r) && arr ! (lo + step) <= x
                                                  then lo + step
                                                  else lo)
                                  l
                                  steps
                in if result <= r && arr ! result == x
                      then Just result
                      else Nothing
  where
    (l, r) = bounds arr
    steps = takeWhile (<= 2 * (r-l)) $ iterate (*2) 1
-}
