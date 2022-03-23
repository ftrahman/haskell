module Set(Set(),
  size,
  empty,
  singleton,
  insert,
  fromList,
  delete,
  member,
  elems
   ) where

import Prelude (Show,undefined,Bool(True,False),(&&),(||),not,
                Integer,(==),(/=),(<),(>),(<=),(>=),(+),(-),(*),div,mod)
import qualified  Prelude ((++), show)
import Hw02 hiding (insert) 


--COLLABORATED WITH NATHAN LEVY AND MCKENZIE JOYCE
-- a set based on search trees
-- note that the implementation details are hidden inside the module
-- this means that outside the module a bad Set Cannot be defined

data Set e = Set (Ordering e) (SetInner e)

data SetInner e = Null | Node (SetInner e) e (SetInner e) deriving Show

-- someone outside the module can make good sets from the functions we provide
exampleGood :: Set Integer
exampleGood = insert 3 ( insert 2 ( insert 1 (empty intOrd)))

-- only possible to make these mistakes inside the module
exampleBad :: Set Integer
exampleBad = Set intOrd (Node (Node Null 3 Null) 2 (Node Null 2 Null) )


-- The number of elements in the set.
size :: Set e -> Integer
size (Set (Ordering compareInteger) Null) = 0
size (Set (Ordering compareBool) Null) = 0
size (Set (Ordering compareInteger) (Node x _ Null)) = 1 + (size (Set (Ordering compareInteger) x))
size (Set (Ordering compareInteger)(Node Null _ z)) = 1 + (size (Set (Ordering compareInteger) z))
size (Set (Ordering compareInteger)(Node x _ z)) = 1+ (size (Set (Ordering compareInteger) x)) +  (size (Set (Ordering compareInteger) z))

-- The empty set.
empty ::  (Ordering e) -> Set e
empty (Ordering compareInteger) = Set (Ordering compareInteger) (Null)
empty (Ordering compareBool) = Set (Ordering compareBool) (Null)

-- Create a singleton set.
singleton :: Ordering e -> e -> Set e
singleton (Ordering compareInteger) n = (Set (Ordering compareInteger) (Node Null n Null))
singleton (Ordering compareBool) n = (Set (Ordering compareBool) (Node Null n Null))


-- Insert an element in a set. If the set already contains an element equal to the given value, it is replaced with the new value.
insert :: e -> Set e -> Set e
insert n (Set (Ordering compareInteger) m) = Set (Ordering compareInteger) (insertHelper m)
  where
    insertHelper (Node x y z)  = case (compareInteger n y) of
       GreaterThan -> Node x y (insertHelper z)
       LessThan -> Node (insertHelper x) y z
       EqualTo -> Node x n z
    insertHelper Null = (Node Null n Null)
 
-- Create a set from a list of elements.
fromList :: Ordering e -> List e -> Set e
fromList (Ordering compareInteger) Nil = Set (Ordering compareInteger) Null
fromList (Ordering compareInteger) (Cons n Nil) = insert n (empty (Ordering compareInteger))
fromList (Ordering compareInteger) (Cons n nList) = insert n (fromList (Ordering compareInteger) nList)

--  The elements of a set in ascending order.
elems  :: Set e -> List e
elems = undefined

-- Is the element in the set?
member :: e -> Set e -> Bool
member = undefined


-- ungraded challenge problem:
-- Delete an element from a set.
delete :: e -> Set e -> Set e
delete = undefined


-- ignore this for now, just so it is pretty in your repl
instance Show e => Show (Set e) where 
  show (Set _ inner) = (Prelude.++) ((Prelude.++) "Set ??? ("  (Prelude.show  inner)) ")"
  
  