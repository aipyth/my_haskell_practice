import LinearAlgebra
import Data.List

allBut :: Eq a => a -> [a] -> [a]
allBut a []     = []
allBut a (x:xs) = if a == x then allBut a xs else x : allBut a xs

lowestLeftmost (Point2D ax ay) (Point2D bx by)
              | ay == by = if ax < bx then LT else GT
              | ay > by  = GT
              | ay < by  = LT


grahamScan :: [Point] -> [Point]
grahamScan points = mainloop stack
          where s = minimumBy lowestLeftmost points
                others = allBut s points
                ox = Vector2D 1 0
                angleComp a b = compare
                                  (cosOfVectors (Vector2D (x a - x s) (y a - y s)) ox)
                                  (cosOfVectors (Vector2D (x b - x s) (y b - y s)) ox)
                stack = s : reverse (sortBy angleComp others)
                mainloop [a, b]     = [a, b]
                mainloop (a:b:c:ds) = if directionTurn a b c == RightTurn
                                      then a : mainloop (c:ds)
                                      else a : mainloop (b:c:ds)
                mainloop a          = a
                
