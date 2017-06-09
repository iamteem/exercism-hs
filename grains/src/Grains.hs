module Grains (square, total) where

import Data.Maybe (catMaybes)

square :: Integer -> Maybe Integer
square n
  | n > 0 && n <= 64 = Just $ 2 ^ (n - 1)
  | otherwise = Nothing

total :: Integer
total = sum $ catMaybes squares
  where squares = map square [1..64]
