module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | otherwise = case sumDivisors `compare` n of
                  LT -> Just Deficient
                  EQ -> Just Perfect
                  GT -> Just Abundant
  where sumDivisors =  sum $ filter ((==0) . (n `mod`)) [1..(n-1)]
