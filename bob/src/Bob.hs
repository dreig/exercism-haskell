module Bob (responseFor) where

import Data.Char

responseFor :: String -> String
responseFor phrase
  | all isSpace phrase = "Fine. Be that way!"

responseFor phrase = case (question phrase, yelling phrase) of
  (True, True) -> "Calm down, I know what I'm doing!"
  (True, _)    -> "Sure."
  (_, True)    -> "Whoa, chill out!"
  _            -> "Whatever."
  where question str = let lastChar = dropWhile isSpace (reverse str)
                        in not (null lastChar) && head lastChar == '?'
        yelling str = any (`elem` ['A'..'Z']) str && not (any (`elem` ['a'..'z']) str)
