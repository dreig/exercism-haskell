module Matrix
    ( Matrix
    , cols
    , column
    , flatten
    , fromList
    , fromString
    , reshape
    , row
    , rows
    , shape
    , transpose
) where

import Data.Vector (Vector, (!))
import qualified Data.Vector as V (unfoldr, fromList, toList, concat, splitAt)

newtype Matrix a = Matrix { getRows :: [Vector a] } deriving (Eq, Show)

cols :: Matrix a -> Int
cols matrix
    | null (getRows matrix) = 0
    | otherwise = length . head $ getRows matrix

column :: Int -> Matrix a -> Vector a
column x = V.fromList . map (!(x-1)) . getRows

flatten :: Matrix a -> Vector a
flatten = V.concat . getRows

fromList :: [[a]] -> Matrix a
fromList = Matrix . map V.fromList

fromString :: Read a => String -> Matrix a
fromString = Matrix . map vecFromString . lines
    where vecFromString = V.fromList . map read . words

reshape :: (Int, Int) -> Matrix a -> Matrix a
reshape (_, y) matrix = Matrix (V.toList $ V.unfoldr maybeSplit $ flatten matrix)
    where
        maybeSplit vec
            | null vec  = Nothing
            | otherwise = Just (V.splitAt y vec)

row :: Int -> Matrix a -> Vector a
row x matrix = getRows matrix !! (x -1)

rows :: Matrix a -> Int
rows = length . getRows

shape :: Matrix a -> (Int, Int)
shape = (,) <$> rows <*> cols

transpose :: Matrix a -> Matrix a
transpose matrix = Matrix $ map (`column` matrix) [1 .. cols matrix]
