module Raindrops (convert) where

convert :: Int -> String
convert n =
  if null sound
    then show n
    else sound
  where pling = if n `mod` 3 == 0 then "Pling" else ""
        plang = if n `mod` 5 == 0 then "Plang" else ""
        plong = if n `mod` 7 == 0 then "Plong" else ""
        sound = pling ++ plang ++ plong
