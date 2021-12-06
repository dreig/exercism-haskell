module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum . nub . concatMap (takeWhile (<limit) . multiplesOf) $ factors
  where multiplesOf 0 = []
        multiplesOf x = [x,2*x..]

-- alternatives to generate multiples of:
-- map ((flip iterate 0) . ((+) $) [3,5,7,..]
-- multiplesOf factor = scanl1 (+) (repeat factor)
