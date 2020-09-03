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
