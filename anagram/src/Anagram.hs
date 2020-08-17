module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)
import Data.Function (on)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (distinctFrom xs) $ filter (anagramOf xs) xss
  where anagramOf = (==) `on` (sort . map toLower)
        distinctFrom = (/=) `on` map toLower


