module Series (Error(..), largestProduct) where

import Data.Char (isDigit, digitToInt)
import Data.List (find, unfoldr)

data Error = InvalidSpan | InvalidDigit Char deriving (Show, Eq)

largestProduct :: Int -> String -> Either Error Integer
largestProduct size digits | size < 0 || length digits < size = Left InvalidSpan
largestProduct 0 _ = Right 1
largestProduct size digits = case find (not . isDigit) digits of
  (Just a) -> Left (InvalidDigit a)
  Nothing -> Right $ fromIntegral . maximum . map product . unfoldr (chunks size) . map digitToInt $ digits
  where
    chunks n xs
      | n <= 0 || length xs < n = Nothing
      | otherwise = Just (take n xs, drop 1 xs)
