module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz num
  | num <= 0 = Nothing
  | otherwise = Just (fromIntegral $ length $ takeWhile (/=1) (iterate collatzStep num))
  where collatzStep 1 = 1
        collatzStep n
          | odd n = 3*n + 1
          | even n = n `div` 2
