module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub $ concatMap (takeWhile (<limit) . multiplesOf) factors
  where multiplesOf 0 = []
        multiplesOf factor = scanl1 (+) (repeat factor)

-- here's a nicer way to generate multiples of a factor: [x,2*x..]
