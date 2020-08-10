module Pangram (isPangram) where

import Data.Char

isPangram :: String -> Bool
isPangram text =
  let lowerCaseText = map toLower text
    in null [ c | c <- ['a'..'z'], c `notElem` lowerCaseText]
