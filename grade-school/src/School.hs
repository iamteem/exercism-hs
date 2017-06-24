module School (School, add, empty, grade, sorted) where

import qualified Data.Map as M
import Data.List (sort)

type School = M.Map Int [String]

add :: Int -> String -> School -> School
add gradeNum student school = M.insertWith sortedAppend gradeNum [student] school
  where sortedAppend old new = sort $ old ++ new

empty :: School
empty = M.empty

grade :: Int -> School -> [String]
grade gradeNum school =
  case M.lookup gradeNum school of
    Just a -> a
    Nothing -> []

sorted :: School -> [(Int, [String])]
sorted = M.toAscList
