module DNA (toRNA, toRNA') where

toRNA :: String -> Maybe String
toRNA xs = mapM toRNA' xs

toRNA' :: Char -> Maybe Char
toRNA' 'G' = Just 'C'
toRNA' 'C' = Just 'G'
toRNA' 'T' = Just 'A'
toRNA' 'A' = Just 'U'
toRNA' _   = Nothing
