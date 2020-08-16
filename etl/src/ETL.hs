module ETL (transform) where

import Data.Char (toLower)
import qualified Data.Map as M

transform :: M.Map a String -> M.Map Char a
transform legacyData = foldr (uncurry M.insert) M.empty letterPts
  where letterPts = [(toLower letter, pts) | (pts, letters) <- M.toList legacyData, letter <- letters]

