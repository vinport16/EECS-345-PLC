{- haha -}
fib 1 = 1
fib 2 = 1
fib n = (fib (n-1)) + (fib (n-2))

{- factorial cps -}
fact_cps 0 return = return 1
fact_cps n return = fact_cps (n-1) (\v -> return (n*v))

{- let's write mergesort -}

split_cps [] return = return [] []
split_cps [n] return = return [n] []
split_cps lis return = split_cps ((tail . tail) lis) (\a b -> return ((head lis) : a) ((head (tail lis)) : b) )

merge_cps [] lisb return = return lisb
merge_cps lisa [] return = return lisa
merge_cps lisa lisb return =
  if (head lisa) < (head lisb)
    then
      merge_cps (tail lisa) lisb (\v -> return ((head lisa) : v))
    else
      merge_cps lisa (tail lisb) (\v -> return ((head lisb) : v))

mergesort_cps [] return = return []
mergesort_cps [n] return = return [n]
mergesort_cps lis return = split_cps lis (\lisa lisb -> mergesort_cps lisa (\sa -> (mergesort_cps lisb (\sb -> (merge_cps sa sb return) ) ) ) )

{- now we're gonna create types -}

{- create a Coordinate type that represents a point-}
data Coordinate = Coord2 Double Double | Coord3 Double Double Double deriving (Show)
getx (Coord2 x y) = x
getx (Coord3 x y z) = x
gety (Coord2 x y) = y
gety (Coord3 x y z) = y
getz (Coord2 x y) = 0
getz (Coord3 x y z) = z

distance (Coord2 a b) (Coord2 c d) = sqrt((a - c) * (a - c) + (b - d) * (b - d))

distance2 v w = sqrt((getx v - getx w)*(getx v - getx w) + (gety v - gety w)*(gety v - gety w))

distance3 v w = sqrt((getx v - getx w)*(getx v - getx w) + (gety v - gety w)*(gety v - gety w) + (getz v - getz w)*(getz v - getz w))