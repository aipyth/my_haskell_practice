intersperse :: a -> [[a]] -> [a]

intersperse sep xs
           | length xs == 0    = []
           | length xs == 1    = head xs
           | otherwise         = head xs ++ [sep] ++ intersperse sep (tail xs)
