module Bob (responseFor) where

import Data.Char (isSpace, isAlpha, isUpper)

responseFor :: String -> String
responseFor xs
  | isSilence xs = "Fine. Be that way!"
  | isYell xs = "Whoa, chill out!"
  | isQuestion xs = "Sure."
  | otherwise = "Whatever."

isYell xs = let chars = filter isAlpha xs
            in if null chars then False else all isUpper chars
isSilence [] = True
isSilence xs = all isSpace xs
isQuestion xs = let nonSpace = filter (not . isSpace) xs
                in if null nonSpace then False else last nonSpace == '?'
