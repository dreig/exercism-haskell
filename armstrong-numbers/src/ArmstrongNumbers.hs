module ArmstrongNumbers (armstrong) where

import Data.List (unfoldr)

armstrong :: Integral a => a -> Bool
armstrong x = sum (map (^power) digits) == x
  where digits = reverse $ unfoldr (\n -> if n == 0 then Nothing else Just (n `mod` 10, n `div` 10)) x
        power = length digits
