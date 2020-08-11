module CollatzConjecture (collatz) where

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = []
collatzChain n
  | even n  = n : collatzChain (n `div` 2)
  | odd n   = n : collatzChain (3*n + 1)



collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | otherwise = Just (fromIntegral $ length (collatzChain n))
