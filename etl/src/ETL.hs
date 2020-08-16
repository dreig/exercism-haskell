module ETL (transform) where

import Data.Char (toLower)
import qualified Data.Map as M

transform :: M.Map a String -> M.Map Char a
transform legacyData = M.fromList [(toLower letter, pts) | (pts, letters) <- M.toList legacyData, letter <- letters]

