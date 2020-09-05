module Brackets (arePaired) where

isBracket :: Char -> Bool
isBracket = (`elem` "(){}[]")

arePaired :: String -> Bool
arePaired = go [] . filter isBracket
  where
    go stack "" = null stack
    go arr (x:xs)
      | openingBracket x     = go (x:arr) xs
      | null arr             = False
    go (y:ys) (x:xs)
      | matchingBrackets y x = go ys xs
      | otherwise            = False
    openingBracket = (`elem` "({[")
    matchingBrackets = curry (`elem` zip "({[" ")}]")
