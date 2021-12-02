module Luhn (isValid) where

import Data.Char (isSpace, digitToInt)

stripSpace :: String -> String
stripSpace = filter (not . isSpace)

doubleDigit :: (Num a, Ord a) => a -> a
doubleDigit x = if x >= 5 then 2*x - 9 else 2*x

isValid :: String -> Bool
isValid str
  | length digits <= 1                = False
  | sum doublesDigits `mod` 10 == 0   = True
  | otherwise                         = False
  where digits = reverse . map digitToInt . stripSpace $ str
        pairedDigits = zip digits (cycle [False, True])
        doublesDigits = map (\(dig,b)-> if b then doubleDigit dig else dig) pairedDigits
