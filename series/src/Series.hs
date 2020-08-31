module Series (slices) where

import Data.Char (digitToInt)

slices :: Int -> String -> [[Int]]
slices n str = slices' n $ map digitToInt str
  where
    slices' 0 xs = replicate (length xs + 1) []
    slices' n xs
      | n > length xs = []
      | otherwise     = take n xs : slices' n (tail xs)
