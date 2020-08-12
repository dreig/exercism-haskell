module DNA (toRNA) where

-- TODO: use fmap to naturally combine / process the Either type
toRNA :: String -> Either Char String
toRNA str
  | not (null firstInvalidChar) = Left (head firstInvalidChar)
  where firstInvalidChar = take 1 (dropWhile (`elem` "CGTA") str)
toRNA str = Right (validToRNA str)

validToRNA :: String -> String
validToRNA "" = ""
validToRNA (x:xs)
  | x == 'G' = 'C' : validToRNA xs
  | x == 'C' = 'G' : validToRNA xs
  | x == 'T' = 'A' : validToRNA xs
  | x == 'A' = 'U' : validToRNA xs
  | otherwise = error "this should never happen"
