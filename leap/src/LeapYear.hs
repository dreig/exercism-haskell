module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year = case (year `mod` 4, year `mod` 100, year `mod` 400) of
  (0, _, 0) -> True
  (0, 0, _) -> False
  (0, _, _) -> True
  (_, _, _) -> False
