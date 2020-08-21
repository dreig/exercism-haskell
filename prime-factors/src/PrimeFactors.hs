module PrimeFactors (primeFactors) where

primes :: [Integer]
primes = filterPrimes (2:[3,5..])
  where filterPrimes (p:xs) = p: filterPrimes (filter ((/=0) . (`mod` p)) xs)

primeFactors :: Integer -> [Integer]
primeFactors n = go n primes
  where go 1 _ = []
        go x (p:xs)
          | p * p > x = [x]
          | x `mod` p == 0 = p : go (x `div` p) (p:xs)
          | otherwise = go x xs
