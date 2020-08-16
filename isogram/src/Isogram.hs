module Isogram (isIsogram) where

import Data.Char (isAlpha, toLower)
import Data.List (nub)

isIsogram :: String -> Bool
isIsogram phrase = length letters == length (nub letters)
  where letters = map toLower $ filter isAlpha phrase
