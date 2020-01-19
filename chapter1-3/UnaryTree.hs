data Tree a = Branch a (Maybe (Tree a)) (Maybe (Tree a))
              deriving (Show)

seed = Branch "seed1" (Nothing) (Nothing)
seed2 = Branch "seed2" (Just seed) (Nothing)
