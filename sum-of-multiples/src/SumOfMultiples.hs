module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub $ concat [limitedMultiplesOf factor | factor <- factors]
  where multiplesOf 0 = []
        multiplesOf factor = scanl1 (+) (repeat factor)
        limitedMultiplesOf = takeWhile (<limit) . multiplesOf
