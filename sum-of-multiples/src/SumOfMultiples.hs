module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum multiples
  where multiples = [ c | c <- [1..(limit-1)], any ((==0) . (c `mod`)) nonZeroFactors]
        nonZeroFactors = filter (/=0) factors
