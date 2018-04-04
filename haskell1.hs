{- this is a comment lol haha -}
{- run these by doing ghci and then :load file.hs -}

{-factorial-}
factorial1 n = 
  if n == 0
		then
			1
		else
			n * factorial1 (n - 1)

factorial2 :: Int -> Int
factorial2 0 = 1
factorial2 n = n * factorial2 (n - 1)

factorial3 :: Int -> Int
factorial3 =
	\n -> if n == 0
					then
						1
					else
						n * factorial3 (n - 1)

{- append time -}
myappend lisa lisb =
	if lisa == []
		then
			lisb
		else
			{- (cons (car lisa) (myappend (cdr lisa) lisb)) -}
			{- car is head, cdr is tail, and cons is : -}
			(head lisa) : (myappend (tail lisa) lisb)


myreverse1 lis =
	if lis == []
		then
			lis
		else
			myappend (myreverse1 (tail lis)) ((head lis) : [])

myreverse2 [] = []
myreverse2 lis = ((myreverse2 (tail lis)) `myappend` ((head lis) : []))


myreverse3 [] = []
{-                            v this dot composes functions (the myreverse3 of the tail of the lis) -}
myreverse3 lis = (((myreverse3 . tail) lis) `myappend` ((head lis) : []))

{- define the types u need: needs to have equivalence,
	 other inputs defined using first input (returns a list of the type of the first input) -}
replaceall :: Eq a => a -> a -> [a] -> [a]
replaceall x y [] = []
replaceall x y lis =
	if head lis == x
		then
			y : replaceall x y (tail lis)
		else
			(head lis) : replaceall x y (tail lis)

merge [] lisb = lisb
merge lisa [] = lisa
merge lisa lisb =
	if (head lisa) > (head lisb)
		then
			(head lisb) : (merge lisa (tail lisb))
		else
			(head lisa) : (merge (tail lisa) lisb)































