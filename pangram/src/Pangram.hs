module Pangram (isPangram) where

isPangram :: String -> Bool
isPangram text =
  let letters = zip ['a'..'z'] ['A'..'Z']
    in null [ lower | (lower, upper) <- letters, (lower `notElem` text) && (upper `notElem` text) ]
