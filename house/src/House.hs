module House (rhyme, rhymeNum, lineNum) where
import Data.List (intercalate)

data Character = Character { charWhat :: String
                           , charAction :: String
                           }
                  deriving (Eq, Ord, Read, Show)

characters :: [Character]
characters = [  Character { charWhat = "house that Jack built." ,           charAction = "" }
              , Character { charWhat = "malt",                              charAction = "lay in" }
              , Character { charWhat = "rat",                               charAction = "ate" }
              , Character { charWhat = "cat",                               charAction = "killed" }
              , Character { charWhat = "dog",                               charAction = "worried" }
              , Character { charWhat = "cow with the crumpled horn",        charAction = "tossed" }
              , Character { charWhat = "maiden all forlorn",                charAction = "milked" }
              , Character { charWhat = "man all tattered and torn",         charAction = "kissed" }
              , Character { charWhat = "priest all shaven and shorn",       charAction = "married" }
              , Character { charWhat = "rooster that crowed in the morn",   charAction = "woke" }
              , Character { charWhat = "farmer sowing his corn",            charAction = "kept" }
              , Character { charWhat = "horse and the hound and the horn",  charAction = "belonged to" }
              ]

lineNum :: Int -> Int -> String
lineNum num n
  | n > num   = error "line " ++ show n ++ " too large for rhyme " ++ show num
  | n == num  = "This is the " ++ charWhat (characters !! num)
  | otherwise = let char     = characters !! (n + 1)
                    charPrev = characters !! n
                in "that "  ++ charAction char ++ " the " ++ charWhat charPrev

rhymeNum :: Int -> String
rhymeNum num = unlines $ map (lineNum num) (reverse [0..num])

rhymes' :: [String]
rhymes' = map rhymeNum [0..length characters - 1]

rhyme :: String
rhyme = intercalate "\n" rhymes'
