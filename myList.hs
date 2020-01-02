data List a = Cons a (List a)
            | Nil
              deriving (Show) 

toList (x:xs) = Cons x (toList xs)
toList []     = Nil

fromList (Cons a b)   = a : fromList b
fromList Nil = []
