module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter ((/= map toLower xs) . map toLower) possibleMatches
  where possibleMatches = filter ((== canonicalOriginal) . canonicalRep) xss
        canonicalRep = sort . map toLower
        canonicalOriginal = canonicalRep xs
