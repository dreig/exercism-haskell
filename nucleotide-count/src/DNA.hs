module DNA (
  nucleotideCounts,
  Nucleotide(..)
) where

import qualified Data.Map as M

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

parseNucleotide :: Char -> Either String Nucleotide
parseNucleotide c = case c of
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  _ -> Left $ "Char " ++ show c ++ " is not a valid Nucleotide."

nucleotideCounts :: String -> Either String (M.Map Nucleotide Int)
nucleotideCounts xs = case traverse parseNucleotide xs of
  (Left err) -> Left err
  (Right nucleotides) -> Right $ M.fromListWith (+) $ zip nucleotides (repeat 1)


