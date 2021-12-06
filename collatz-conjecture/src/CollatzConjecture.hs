module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz num
  | num <= 0 = Nothing
  | num == 1 = Just 0
  | otherwise = (+1) <$> if odd num then collatz (3*num + 1) else collatz (num `div` 2)
