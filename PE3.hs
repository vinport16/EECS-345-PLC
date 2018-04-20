-- programming exercize 3: vlp25

-- Create the function removedups that takes a list and removes duplicate elements
removedups [] = []
removedups [a] = [a]
removedups lis = if (head lis) == (head (tail lis)) then (removedups (tail lis)) else (head lis) : (removedups (tail lis))


-- Create a continuation passing version, removedups_cps
removedups_cps [] return = return []
removedups_cps [a] return = return [a]
removedups_cps lis return = if (head lis) == (head (tail lis)) then (removedups_cps (tail lis) return) else (removedups_cps (tail lis) (\x -> return ((head lis) : x)))


-- Create a type that allows us to have nested lists. Your type should have two kinds of values, elements and sublists.
-- For example, the following will be a valid list:
-- [Element 1,Element 3,Sublist [Element 4,Sublist [Sublist [Element 5],Sublist []]],Element 6]
data MyList i = Element i | Sublist [MyList i] deriving(Show,Eq)


-- Create the function gremovedups that takes a list containing elements and sublists and returns a list with the same structure,
-- but if any "element" is preceded by an identical element, that element is removed.
-- gremovedups [Element 4,Element 4,Element 5,Sublist [Element 6,Element 6,Sublist[Element 8,Element 8,Element 8]], Element 5,Element 5]

gremovedups [] = []
gremovedups [(Element i)] = [(Element i)]
gremovedups [(Sublist i)] = [Sublist (gremovedups i)] 
gremovedups list =
  if (head list == head (tail list))
    then (gremovedups (tail list))
    else (gremovedups [(head list)]) ++ (gremovedups (tail list))


-- write a function bubbledown that takes a Tree as input. If the element stored in the root is larger than either children,
-- swap the element with the smaller child, and recurse on the child you swapped the element with.
-- The recursion should stop when either you reach a leaf or when the element of the node is smaller than both its children. 
data Tree t = Leaf t | Internal t (Tree t) (Tree t) deriving(Show)

getvalue (Leaf t) = t
getvalue (Internal t l r) = t

insert v (Internal t l r) = (Internal v l r)
insert v (Leaf t) = (Leaf v)

bubbledown (Leaf t) = (Leaf t)
bubbledown (Internal t l r) = 
  if ( ((t > getvalue l) || (t > getvalue r) ) && ((getvalue l) < (getvalue r)))
    then
      (Internal (getvalue l) (bubbledown (insert t l)) r)
    else
      if ( ((t > getvalue l) || (t > getvalue r) ) && ((getvalue l) > (getvalue r)))
        then
          (Internal (getvalue r) l (bubbledown (insert t r)))
        else
          (Internal t l r)


-- Using the Maybe monad of Haskell, create a function called checkcons that has the following type: 
-- checkcons :: Maybe a -> Maybe [a] -> (a -> Bool) -> Maybe [a]
-- The function takes a Maybe value of some type, a Maybe list of the same type (as a monad),
-- and a test function and returns a Maybe list of the same type.
-- If either Maybe is Nothing, the result is Nothing.
-- If the first Maybe value passes the test function,
-- the result has the first element cons'd onto the front of the list.
-- Otherwise the result is Nothing. 

checkcons (Nothing) (Just list) f = Nothing
checkcons (Just a) (Nothing) f = Nothing
checkcons (Just a) (Just list) f = 
  if (f a)
    then
      (Just (a : list))
    else
      (Nothing)


-- Using checkcons create a function checklist that takes a list and a function and returns Nothing
-- if the elements in the list fail to past the function and the list (embedded in a Maybe)
-- if all the elements pass. 

checklist [] f = (Just [])
checklist list f = checkcons (Just (head list)) (checklist (tail list) f) f


-- Create a list monad that generalizes a list. This will not be a Haskell Monad type, but instead one of our own
-- creation like the Value type from lecture. For example, the following is a valid "list".
-- -- Pair 4 (Pair 5 (Pair 6 Null))
-- Then create a binding function lbind and a return function lreturn to make a list monad. The code should work so that
-- -- (Pair 4 (Pair 5 (Pair 6 Null))) `lbind` (\x -> lreturn (2 * x))   =>   Pair 8 (Pair 10 (Pair 12 Null))

data List t = Pair t (List t) | Null deriving (Show)

lreturn x = Pair x

lbind (Null) f = (Null)
lbind (Pair x l) f = ( f x (lbind l f))














