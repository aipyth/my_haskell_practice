data Tree a = Node a (Tree a) (Tree a)
          | Empty
            deriving (Show)

treeHeight Empty                  = 0
treeHeight ( Node _ Empty Empty ) = 1
treeHeight ( Node _ Empty right ) = 1 + treeHeight right
treeHeight ( Node _ left  Empty ) = 1 + treeHeight left
treeHeight ( Node _ left  right ) = 1 + max r_height l_height
           where r_height = treeHeight right
                 l_height = treeHeight left
