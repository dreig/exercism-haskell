module DNA (
  nucleotideCounts,
  Nucleotide(..)
) where

import qualified Data.Map as M
import Control.Monad (foldM)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)
type NucleotideCount = M.Map Nucleotide Int

parseNucleotide :: Char -> Either String Nucleotide
parseNucleotide c = case c of
  'A' -> Right A
  'C' -> Right C
  'G' -> Right G
  'T' -> Right T
  _ -> Left $ "Char " ++ show c ++ " is not a valid Nucleotide."

initialMap :: NucleotideCount
initialMap = M.fromList
  [(A,0)
  ,(C,0)
  ,(G,0)
  ,(T,0)
  ]

nucleotideCounts :: String -> Either String NucleotideCount
nucleotideCounts xs = foldM insertM initialMap $ map parseNucleotide xs
  where insertM = fmap . flip (M.adjust (+1))
  {-
    `insertM` takes two arguments, the accumulator Map and the Either-wrapped Nucleotide
              and returns the updated accumulator Map (if the Either-wrapped Nucleotide is a Right-value)

    :type insertM
    :: (Functor f, Ord a1, Num a2) =>
        M.Map a1 a2 -> f a1 -> f (M.Map a1 a2)

    we have to `flip` (M.adjust (+1)) since its arguments are reversed from what `foldM` expects
    :type M.adjust (+1)
    :: (Ord k, Num a) => k -> M.Map k a -> M.Map k a
  -}
