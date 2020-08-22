module Base (Error(..), rebase) where

import Data.List

data Error a = InvalidInputBase | InvalidOutputBase | InvalidDigit a
    deriving (Show, Eq)

rebase :: Integral a => a -> a -> [a] -> Either (Error a) [a]
rebase inputBase outputBase inputDigits
  | inputBase <= 1 = Left InvalidInputBase
  | outputBase <= 1 = Left InvalidOutputBase
  | otherwise = case find (\d -> d < 0 || d >= inputBase) inputDigits of
    (Just a) -> Left (InvalidDigit a)
    Nothing  -> Right $ toBase number outputBase
  where number = fromBase inputBase inputDigits
        fromBase base = foldl' (\acc d -> acc * base + d) 0
        toBase 0 _    = []
        toBase n base = reverse $ unfoldr (`maybeDiv` base) n
        maybeDiv 0 _ = Nothing
        maybeDiv n base = let (x,y) = (n `quotRem` base) in Just (y,x)
