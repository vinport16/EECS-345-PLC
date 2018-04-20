data Coordinate t = Coord0 | Coord1 t | Coord2 t t | Coord3 t t t deriving(Show)

{- wrong:
instance (Eq t) Eq (Coordinate t) where
  (Coord0) == (Coord0) = True
  (Coord0) == (Coord1 a) = a == 0
  (Coord0) == (Coord2 a b) = a == 0 && b == 0
  (Coord0) == (Coord3 a b c) = a == 0 && b == 0 && c == 0
  (Coord1 a) == (Coord0) = (Coord0) == (Coord1 a)
  (Coord1 a) == (Coord1 b) = a == b
  (Coord1 a) == (Coord2 b c) = a == b && c == 0
  (Coord1 a) == (Coord3 b c d) = a == b && 
  (Coord2 a b) == (Coord2 c d) = a == c && b == d
  (Coord3 a b c) == (Coord2 d e f) = a == d && b == e && c == f
-}

  {- a == b = (getx a == getx b) && (gety a == gety b) && (getz a == getz b) -}

getx (Coord0) = 0
getx (Coord1 x) = x
getx (Coord2 x y) = x
getx (Coord3 x y z) = x

gety (Coord0) = 0
gety (Coord1 x) = 0
gety (Coord2 x y) = y
gety (Coord3 x y z) = y

getz (Coord0) = 0
getz (Coord1 x) = 0
getz (Coord2 x y) = 0
getz (Coord3 x y z) = z

distance c1 c2 = sqrt((getx c1 - getx c2)*(getx c1 - getx c2)+(gety c1 - gety c2)*(gety c1 - gety c2)+(getz c1 - getz c2)*(getz c1 - getz c2))

{- addition: use this like (Coord2 4 5) |+ (Coord0) -}
(|+) (Coord3 x y z) c2 = Coord3 (x + getx c2) (y + gety c2) (z + getz c2)
(|+) c1 (Coord3 x y z) = Coord3 (getx c1 + x) (gety c1 + y) (getz c1 + z)

(|+) (Coord2 x y) c2 = Coord2 (x + getx c2) (y + gety c2) 
(|+) c1 (Coord2 x y) = Coord2 (getx c1 + x) (gety c1 + y) 

(|+) (Coord1 x) c2 = Coord1 (x + getx c2) 
(|+) c1 (Coord1 x) = Coord1 (getx c1 + x) 

(|+) Coord0 Coord0 = Coord0