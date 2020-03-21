module LinearAlgebra where
data Point = Point2D { x :: Float, y :: Float }
             deriving (Show, Eq)

data Vector = Vector2D { i :: Float, j :: Float }
            | Vector3D { i :: Float, j :: Float, k :: Float }
              deriving (Show, Eq)

data Direction = LeftTurn
               | RightTurn
               | NoTurn
                 deriving (Show, Eq)

toVector :: Point -> Point -> Vector
toVector a b = Vector2D (x b - x a) (y b - y a)

crossProduct :: Vector -> Vector -> Vector
crossProduct a b = Vector3D 0 0 (i a * j b - i b * j a)

dotProduct :: Vector -> Vector -> Float
dotProduct a b = i a * i b + j a * j b

vectorLength :: Vector -> Float
vectorLength a = sqrt (i a ** 2 + j a ** 2)

cosOfVectors :: Vector -> Vector -> Float
cosOfVectors a b = dotProduct a b / ( vectorLength a * vectorLength b )


directionTurn :: Point -> Point -> Point -> Direction
directionTurn a b c
         | turnVectorLength >  0   = LeftTurn
         | turnVectorLength <  0   = RightTurn
         | turnVectorLength == 0   = NoTurn
         where turnVectorLength = k (crossProduct (toVector b a) (toVector c b))

directionTurnList :: [Point] -> [Direction]
directionTurnList (a:b:c:ts)
                 | null ts    = [directionTurn a b c]
                 | otherwise  = [directionTurn a b c] ++ directionTurnList (b:c:ts)
