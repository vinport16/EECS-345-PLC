{- monads ? -}
{- parameterized type with at least two constructors -}
{- 2 functions: return: takes value and creates a monad "storing" the value;
								bind: takes a monad and a function and applies the function to the value in the monad -}

data Value t = Value t | NoValue deriving (Show, Eq)

-- return function for Value
myreturn x = Value x

-- bind function for Value
mybind:: Value t -> (t -> Value t1) -> Value t1
mybind (Value x) f = f x
mybind NoValue _ = NoValue
-- the underscore above means "any input, i'm not gonna use it"

-- things u can do:
--  mybind (Value 10) (\x -> (mybind (Value 20) (\y -> myreturn (x+y))))
-- --> Value 30
-- (Value 10) `mybind` (\x -> (Value 20) `mybind` (\y -> myreturn (x + y)))
-- --> Value 30


-- function to add two monads
(+++) vx vy = vx `mybind` (\x -> vy `mybind` (\y -> myreturn (x + y)))

-- (Value 100) +++ (Value 1)
-- --> Value 101

-- divide two monads
(//) vx vy = vx `mybind` (\x -> vy `mybind` (\y -> if y == 0 then NoValue else myreturn (x / y)))

-- sqrt
vsqrt vx = vx `mybind` (\x -> if x < 0 then NoValue else myreturn (sqrt x))
-- vsqrt (Value 20)
-- --> Value 4.47213595499958
-- vsqrt (Value (-20))
-- --> NoValue

-- do any infix function to two values
vapp vx f vy = vx `mybind` (\x -> vy `mybind` (\y -> myreturn (f x y)))
-- vapp (Value 100) (*) (Value 2)
-- --> Value 200


{- Haskell has many built-in monads: Maybe
		data Maybe t = Just t | Nothing
		return  is just "return"
		bind is ">>=" -}
(++++) mx my = mx >>= (\x -> my >>= (\y -> return(x+y)))
(///) mx my = mx >>= (\x -> my >>= (\y -> if y == 0 then Nothing else return (x / y)))

mapp mx f my = mx >>= (\x -> my >>= (\y -> return(f x y)))
-- use a shortcut that haskell provides: do
mapp2 mx f my = do
  x <- mx
  y <- my
  return (f x y)

msqrt mx = do
  x <- mx
  if x < 0 then Nothing else return (sqrt x)

-- so. lists are monads too.
-- [1,2,3,4,5] >>= (\x -> [x,x])
-- --> [1,1,2,2,3,3,4,4,5,5]
-- [1,2,3,4,5] >>= (\x -> [x*2])
-- --> [2,4,6,8,10]
-- [1,2,3,4,5] >>= (\x -> ["hi"])
-- --> ["hi","hi","hi","hi","hi"]
-- ["abc", "ur", "a", "nerd", "yolo", "1776_4_evah"] >>= (\x -> [length x])
-- --> [3,2,1,4,4,11]
-- [1,2,3,4,5] >>= (\x -> [show x])
-- --> ["1","2","3","4","5"]
























