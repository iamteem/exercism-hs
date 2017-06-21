module DNA (nucleotideCounts) where

import Data.Map (Map, fromList, updateLookupWithKey)

type NucleotideCountMap = Map Char Int

nucleotideCounts :: String -> Either String NucleotideCountMap
nucleotideCounts xs = processString xs $ Right initialMap

initialMap = fromList $ zip "ACGT" $ repeat 0

processString _ l@(Left _) = l
processString [] r@(Right _) = r
processString (c:xs) (Right map) = processString xs $ incrChar c map

incrChar :: Char -> NucleotideCountMap -> Either String NucleotideCountMap
incrChar c map =
  let res = updateLookupWithKey incr c map
      incr _ i = Just $ i + 1
  in case res of
        (Nothing, _)  -> Left "Invalid strand"
        (Just _, map) -> Right map
