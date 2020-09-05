module Atbash (decode, encode) where

import Data.List.Split (chunksOf)
import Data.Char (ord, chr, toLower, isAlpha, isAlphaNum)

convert :: Char -> Char
convert char
  | isAlpha char = chr $ ord 'z' + ord 'a' - ord char
  | otherwise    = char

normalize :: String -> String
normalize =  map toLower . filter isAlphaNum

decode :: String -> String
decode = map convert . normalize

encode :: String -> String
encode = unwords . chunksOf 5 . map convert . normalize
