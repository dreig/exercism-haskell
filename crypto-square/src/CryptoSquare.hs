module CryptoSquare (encode) where

import Data.List (transpose)
import Data.List.Split (chunksOf)
import Data.Char (isAlphaNum, toLower)

calcDimensions :: Int -> (Int, Int)
calcDimensions n
  | a*a == n    = (a,a)
  | a*(a+1) >=n = (a, a+1)
  | otherwise   = (a+1, a+1)
  where
    a = floor . sqrt . fromIntegral $ n

normalize :: String -> String
normalize = map toLower . filter isAlphaNum

encode :: String -> String
encode xs
  | null xs' = ""
  | otherwise = unwords . transpose . chunksOf c $ xs' ++ replicate padding ' '
  where xs' = normalize xs
        (r,c) = calcDimensions . length $ xs'
        padding = r*c - length xs'
