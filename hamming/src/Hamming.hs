module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
  | length xs == length ys = distance' xs ys
  | otherwise = Nothing

distance' :: String -> String -> Maybe Int
distance' [] [] = Just 0
distance' (x:xs) (y:ys)
  | x == y = distance' xs ys
  | otherwise = (+1) <$> distance' xs ys
