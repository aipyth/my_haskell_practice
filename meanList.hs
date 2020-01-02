meanList [] = Nothing
meanList xs = Just (sum xs / fromIntegral (length xs))
