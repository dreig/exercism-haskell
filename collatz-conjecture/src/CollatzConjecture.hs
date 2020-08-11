module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz num
  | num <= 0 = Nothing
  | otherwise = Just (step num 0)
    where
  step n acc
    | n == 1 = acc
    | odd n  = step (3*n+1) (acc + 1)
    | even n = step (n `div` 2) (acc + 1)
