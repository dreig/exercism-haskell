module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as M
import Data.List (partition, group, sort)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideFromChar :: Char -> Nucleotide
nucleotideFromChar c = read([c])

nucleotideCounts :: String -> Either String (M.Map Nucleotide Int)
nucleotideCounts xs =
  case partition (`elem` "ACGT") xs of
    ([], [])  -> Right $ M.fromList defaultNucleotideCountList
    (_, (x:_)) -> Left "There are non-nucleotide elemjents in the string."
    (nucleotides, []) -> let nucleotideCountList = map (\str -> (nucleotideFromChar(head str), length str)) $ group (sort nucleotides)
                         in Right $ M.fromList (defaultNucleotideCountList ++ nucleotideCountList)
  where defaultNucleotideCountList = [(A,0), (C,0), (G,0), (T, 0)]
