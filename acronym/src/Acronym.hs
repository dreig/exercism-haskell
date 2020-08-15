module Acronym (abbreviate) where

import Data.Char (isAlpha, isUpper, toUpper)

initials :: String -> String
initials [] = []
initials word@(x:xs)
  | all isUpper word = [x]
  | otherwise = toUpper x : filter isUpper xs

abbreviate :: String -> String
abbreviate xs = concatMap initials $ words $ map nonAlphaToSpace xs
  where nonAlphaToSpace c = if isAlpha c || '\'' == c then c else ' '

-- TODO: is there a nice way to define a point-free function that does what nonAlphaToSpace does?
{-
  possible approaches?
  shouldNotChange :: Char -> Bool
  shouldNotChange c = any ($ c) [isAlpha, (=='\'')]
  shouldNotChange = (||) <$> isAlpha <*> (=='\n')

  nonAlphaToSpace c = if shouldNotChange c then c else ' '
-}
