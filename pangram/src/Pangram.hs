module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text =
  let lowerCaseText = map toLower text
      inText c = c `elem` lowerCaseText
    in all inText ['a'..'z']
