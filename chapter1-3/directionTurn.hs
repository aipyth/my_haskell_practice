data Point2D   = Point2D { x :: Float, y :: Float }
                 deriving (Show)
data Vector2D  = Vector2D Float Float
                 deriving (Show)
data Vector3D  = Vector3D Float Float Float
                 deriving (Show)
data Direction = LeftTurn
               | RightTurn
               | NoTurn
                 deriving (Show, Eq)


toVector2D (Point2D ax ay) (Point2D bx by) = Vector2D (bx - ax) (by - ay)
crossProduct2D (Vector2D x1 x2) (Vector2D y1 y2) = Vector3D 0 0 (x1 * y2 - x2 * y1)
trdVector3DCoord (Vector3D x1 x2 x3) = x3

directionTurn :: Point2D -> Point2D -> Point2D -> Direction
directionTurn a b c
         | length_turn_vector > 0    = LeftTurn
         | length_turn_vector < 0    = RightTurn
         | length_turn_vector == 0   = NoTurn
         where length_turn_vector = trdVector3DCoord (crossProduct2D (toVector2D b a) (toVector2D b c))

directionTurnList :: [Point2D] -> [Direction]
directionTurnList (a:b:c:ts)
                 | null ts    = [directionTurn a b c]
                 | otherwise  = [directionTurn a b c] ++ directionTurnList (b:c:ts)
