module ETL (transform) where

import Data.Map (Map, fromList, toList)
import Data.Char (toLower)

transform :: Map a String -> Map Char a
transform legacyData = fromList $ concatMap transform' $ toList legacyData
  where transform' (points, letters) = map (\c -> (toLower c, points)) letters
