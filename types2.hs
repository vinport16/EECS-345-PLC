{- we out here making trees -}

data LTree t = Leaf t | Internal t [LTree t] [LTree t] deriving(Show)

data Tree t = Leaf2 t | Internal2 t (Tree t) (Tree t) deriving(Show)

data GenTree t = GLeaf t | GInternal t [GenTree t] deriving(Show)

linorder (Leaf a) = [a]
linorder (Internal a [l] [r]) = (linorder l) ++ (a : (linorder r))

linorder2 =
  \t -> 
    case t of
      Leaf a -> [a]
      Internal a [l] [r] -> (linorder2 l) ++ (a : (linorder2 r))

{- preorder -}

preorder (Leaf a) = [a]
preorder (Internal a [l] [r]) = (a : (preorder l)) ++ (preorder r)

preorder2 =
  \t -> 
    case t of
      Leaf a -> [a]
      Internal a [l] [r] -> (a : (preorder l)) ++ (preorder r)

{- applyinorder takes a tree and a function and produces a tree with the same structure, but the function is applied to every node element -}
applyinorder (Leaf l) f = Leaf (f l)
applyinorder (Internal i [a] [b]) f = Internal (f i) [(applyinorder a f)] [(applyinorder b f)]

f x = 2*x

{- foldinorder t (:) [] => [0,2,1,4,5,6,8,9,10]
	 foldinorder t (+) 0 => 45 -}

foldinorder f (Leaf l) v = f l v
foldinorder f (Internal i [a] [b]) v = foldinorder f a (f i (foldinorder f b v))
