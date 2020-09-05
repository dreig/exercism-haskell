module Brackets (arePaired) where

import Data.List (foldl')

isBracket :: Char -> Bool
isBracket = (`elem` "(){}[]")

matchingBrackets :: (Char, Char) -> Bool
matchingBrackets = (`elem` zip "({[" ")}]")

-- inspired from https://exercism.io/tracks/haskell/exercises/matching-brackets/solutions/4e50bef28aa9420991df7d2130a1d843
arePaired :: String -> Bool
arePaired = null . foldl' removeMatching [] . filter isBracket
            where
              removeMatching stack x
                | (top:stack') <- stack, matchingBrackets (top,x) = stack'
                | otherwise                                       = x : stack
