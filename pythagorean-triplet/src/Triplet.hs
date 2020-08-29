module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum n = [(x,y,z) | x <- [1..n], y' <- [2 * x + 1 .. n - x], let y = y' - x; z = n - y', x*x + y*y == z*z]
