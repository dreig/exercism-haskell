module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = squareOfSum n - sumOfSquares n
-- Alternative (point-free) defintion
-- difference = (-) . squareOfSum <*> sumOfSquares
-- More info here: http://learnyouahaskell.com/functors-applicative-functors-and-monoids

squareOfSum :: Integral a => a -> a
squareOfSum n = square $ sum [1..n]
-- Alternative (point-free) definition
-- squareOfSum = square . sum . enumFromTo 1

sumOfSquares :: Integral a => a -> a
sumOfSquares n = sum $ map square [1..n]
-- Alternative (point-free) definition
-- sumOfSquares = sum . map square . enumFromTo 1

square :: Integral a => a -> a
square x = x * x
