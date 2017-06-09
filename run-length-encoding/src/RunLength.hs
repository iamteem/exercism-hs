module RunLength (decode, encode) where

import Data.List (group)
import Data.Char (isDigit, isLetter, isSpace)
import Text.ParserCombinators.ReadP

decode :: String -> String
decode encodedText
  | res == [] = ""
  | otherwise = concatMap (\(n, c) -> replicate n c) tuples
  where res = readP_to_S rleparser encodedText
        tuples = (fst . head) res

rleparser :: ReadP [(Int, Char)]
rleparser = do xs <- many compressedLetter
               eof
               return xs

number :: ReadP Int
number = do nums <- many1 $ satisfy isDigit
            return $ read nums

letter :: ReadP Char
letter = do l <- satisfy (\c -> isLetter c || isSpace c)
            return l

compressedLetter :: ReadP (Int, Char)
compressedLetter = do n <- option 1 number
                      c <- letter
                      return (n, c)

encode :: String -> String
encode text = concatMap encodeGroup (group text)

encodeGroup g
  | len > 1 = show len ++ [head g]
  | otherwise = take 1 g
  where len = length g
