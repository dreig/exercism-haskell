module Matrix (saddlePoints) where

import Data.Array (Array, assocs, bounds, (!))

saddlePoints :: Ord e => Array (Int, Int) e -> [(Int, Int)]
saddlePoints matrix = [(x,y) | ((x,y), v) <- assocs matrix, v == rowMaximums !! (x-1), v == colMinimums !! (y-1)]
  where
    (_, (rows, cols)) = bounds matrix
    rowMax n = maximum [matrix ! (n, x) | x <- [1..cols]]
    rowMaximums = map rowMax [1..rows]
    colMin m = minimum [matrix ! (x, m) | x <- [1..rows]]
    colMinimums = map colMin [1..cols]
