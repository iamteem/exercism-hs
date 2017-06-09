module SumOfMultiples (sumOfMultiples) where

import Control.Monad (guard)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum multiples
  where multiples = do x <- [0..(limit - 1)]
                       guard $ any (\f -> x `mod` f == 0) factors
                       return x

