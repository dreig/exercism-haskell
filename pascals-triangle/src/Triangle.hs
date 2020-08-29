module Triangle (rows) where

rows :: Int -> [[Integer]]
rows 0 = []
rows n = take n $ iterate nextRow [1]
        where nextRow r = zipWith (+) (0:r) (r++[0])

