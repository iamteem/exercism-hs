module Pangram (isPangram) where

import Data.Set (isSubsetOf, fromList)
import Data.Char (toLower)

alphabet = fromList ['a'..'z']

isPangram :: String -> Bool
isPangram text = isSubsetOf alphabet $ fromList $ map toLower text
