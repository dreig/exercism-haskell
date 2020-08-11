module Bob (responseFor) where

import Data.Char (isSpace, isLower, isUpper)

responseFor :: String -> String
responseFor phrase
  | all isSpace phrase = "Fine. Be that way!"

responseFor phrase = case (isQuestion phrase, isYelling phrase) of
  (True, True) -> "Calm down, I know what I'm doing!"
  (True, _)    -> "Sure."
  (_, True)    -> "Whoa, chill out!"
  _            -> "Whatever."
  where isQuestion str = let lastChar = dropWhile isSpace (reverse str)
                        in not (null lastChar) && head lastChar == '?'
        isYelling str = any isUpper str && not (any isLower str)
