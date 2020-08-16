module Diamond (diamond) where

diamond :: Char -> Maybe [String]
diamond c
  | c `notElem` ['A'..'Z'] = Nothing
  | otherwise = Just $ topHalf ++ drop 1 (reverse topHalf)
  where topHalf = map (buildLine c) ['A' .. c]

buildLine :: Char -> Char -> String
buildLine endChar curChar =
  map (\c -> if c == curChar then c else ' ') $ reverse ['A' .. endChar] ++ drop 1 ['A' .. endChar]

