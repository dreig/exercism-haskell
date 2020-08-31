module Proverb(recite) where

import Data.List (intercalate)

recite :: [String] -> String
recite [] = ""
recite xs@(x:_) = intercalate "\n" $ recite' xs ++ ["And all for the want of a " ++ x ++ "."]
  where recite' items = zipWith (\a b -> "For want of a " ++ a ++ " the " ++ b ++ " was lost.") items (drop 1 items)
