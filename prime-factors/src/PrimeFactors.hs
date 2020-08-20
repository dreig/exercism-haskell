module PrimeFactors (primeFactors) where

import Data.List(span, filter)

primes :: [Integer]
primes = filterPrimes (2:[3,5..])
  where filterPrimes (p:xs) = p: filterPrimes (filter ((/=0) . (`mod` p)) xs)

primeFactors :: Integer -> [Integer]
primeFactors n = go n primes
  where go 1 _ = []
        go x primes' =
          let (_, (factor : tailPrimes)) = span (\p -> p*p <= x && x `mod` p /= 0) primes'
          in (
            if factor * factor > x
              then [x]
              else let (factors, x')  = repeatFactor factor x []
                   in (factors ++ (go x' tailPrimes))
          )

repeatFactor :: Integer -> Integer -> [Integer] -> ([Integer], Integer)
repeatFactor factor n xs
  | n `mod` factor /= 0 = (xs, n)
  | otherwise = repeatFactor factor (n `div` factor) (factor : xs)
