module ArmstrongNumbers (armstrong) where

armstrong :: Integral a => a -> Bool
armstrong x = sum (map (^power) digits) == x
  where digits = intToDigits x
        power = length digits

intToDigits :: Integral a => a -> [a]
intToDigits 0 = [0]
intToDigits n = intToDigits' n
  where intToDigits' = reverse . map (`mod` 10) . takeWhile (/=0) . iterate (`div` 10)
