module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA = traverse toRNABase

toRNABase :: Char -> Either Char Char
toRNABase c = case c of
  'G' -> Right 'C'
  'C' -> Right 'G'
  'T' -> Right 'A'
  'A' -> Right 'U'
  _   -> Left c
