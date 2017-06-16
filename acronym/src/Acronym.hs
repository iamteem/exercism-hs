module Acronym (abbreviate) where

import Data.Char (toUpper, isUpper, isLower)

abbreviate :: String -> String
abbreviate xs = concatMap getCap $ words xs

getCap [] = []
getCap (t:rest) = map toUpper $ [t] ++ getCap' rest

getCap' [] = []
getCap' (x:[]) = []
getCap' ('-':t:rest) = [t] ++ getCap' rest
getCap' (s:b:rest)
  | isLower s && isUpper b = [b] ++ getCap' rest
  | otherwise = getCap' (b:rest)
