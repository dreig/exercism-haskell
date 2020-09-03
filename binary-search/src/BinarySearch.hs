module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find arr x
  | null arr = Nothing
  | otherwise = let (l,r) = bounds arr
                    m = l + (r-l) `div` 2
                in if arr ! m == x
                      then Just m
                      else if arr ! m > x
                              then find (ixmap (l, m-1) id arr) x
                              else find (ixmap (m+1, r) id arr) x
