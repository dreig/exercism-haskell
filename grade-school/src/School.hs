module School (School, add, empty, grade, sorted) where

import Data.List (sort)
import qualified Data.Map as M
type School = M.Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student school
  | M.member gradeNum school = M.adjust (student :) gradeNum school
  | otherwise = M.insert gradeNum [student] school

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade gradeNum school = case M.lookup gradeNum school of
  Nothing -> []
  (Just students) -> sort students

sorted :: School -> [(Int, [String])]
sorted school = sort [ (gradeNum, sort students) | (gradeNum, students) <- M.toList school ]
