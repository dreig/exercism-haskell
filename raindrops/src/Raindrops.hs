module Raindrops (convert) where

convert :: Int -> String
convert n =
  if null phrase
    then show n
    else phrase
  where numberSounds = zip [3,5,7] ["Pling", "Plang", "Plong"]
        phrase = concatMap snd $ filter ((==0) . (n `mod`) . fst) numberSounds
