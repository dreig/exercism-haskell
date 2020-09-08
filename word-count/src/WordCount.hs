module WordCount (wordCount) where

import qualified Data.Map as M
import Data.Char (isAlphaNum, isSpace, toLower)

normalize :: String -> String
normalize = map toLower . map (\c -> if isValid c then c else ' ')
  where isValid char = or $ [isSpace, isAlphaNum, (=='\'')] <*> pure char
  -- where isValid char = any ($ char) [isSpace, isAlphaNum, (=='\'')]


wordCount :: String -> [(String, Int)]
wordCount = M.toList . foldr (insertWith' (+) 1) M.empty . map removeQuotes . words . normalize
  where insertWith' f a k = M.insertWith f k a
        removeQuotes []            = []
        removeQuotes ('\'':xs)     = removeQuotes xs
        removeQuotes (x:'\'':"")   = x:""
        removeQuotes (x:'\'':y:xs) = x:'\'':y:removeQuotes xs
        removeQuotes (x:xs)        = x:removeQuotes xs


