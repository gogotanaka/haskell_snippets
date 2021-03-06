-- Vector Basis --
type Point = (Float, Float)

subt :: Point -> Point -> Point
subt (x1, y1) (x2, y2) = ((x1 - x2), (y1 - y2))

norm :: Point -> Float
norm (x, y) = sqrt (x ^ 2 + y ^ 2)

inner :: Point -> Point -> Float
inner (x1, y1) (x2, y2) = x1 * x2 + y1 * y2
------------------

type Radius   = Float
data Triangle = Triangle Point Point Point     deriving (Show)
data Square   = Square Point Point Point Point deriving (Show)
data Circle   = Circle Point Radius            deriving (Show)

class Shape a where
  simple  :: a
  rotate  :: a -> a
  surface :: a -> Float

instance Shape Triangle where
  simple                   = Triangle (0, 0) (1, 0) (0, 1)
  rotate  (Triangle x y z) = Triangle z x y
  surface (Triangle x y z) = 0.5 * sqrt ((norm v1) ^ 2 * (norm v2) ^ 2 - (v1 `inner` v2) ^ 2)
    where v1 = subt x y
          v2 = subt x z

instance Shape Square where
  simple                   = Square (0, 0) (1, 0) (1, 1) (0, 1)
  rotate  (Square w x y z) = Square z w x y
  surface (Square w x y z) = surface (Triangle w x y) + surface (Triangle z x y)

instance Shape Circle where
  simple               = Circle (0, 0) 1
  rotate x             = x
  surface (Circle _ r) = pi * r ^ 2
 
