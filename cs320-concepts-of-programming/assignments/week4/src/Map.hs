module Map
  (Map(),empty, insert, toList, fromList, size, member, lookup, delete, update, union, filter,
  null, singleton, elems, keys, difference, adjust, alter
  ) where  -- DO NOT EDIT THESE LINES
import Prelude hiding (filter, lookup, null)
  
  
-- For this problem, you must develop a variation of a dictionary based on BST. 
-- Such a data structure is very useful in a wide variety of applications.

otherwise :: Bool
otherwise = True

data Map k v = Null | Node (Map k v) k v (Map k v)  deriving Show

-- The empty map.
empty :: Map k a
empty = Null

-- Insert a new key and value in the map. If the key is already present in the map, the associated value is replaced with the supplied value
insert :: Ord k => k -> a -> Map k a -> Map k a
insert k a Null = (Node Null k a Null)
insert k a (Node b x y c) | x == k = (Node b k a c)
                          | x < k = (Node b x y (Node Null k a Null))
                          | x > k = (Node (Node Null k a Null) x y c)

--insert k a (Node Null x y c) = (Node (Node Null k a Null) x y c)
--insert k a (Node b _ _ _) = insert(k a b)
--insert k a (Node _ _ _ c) = insert(k a c)


--  Convert to a list of key/value pairs.
toList :: Map k a -> [(k, a)]
toList Null = []
toList (Node b k a _) = (++) (k,a)(toList(b))
toList (Node _ k a c) = (++) (k,a)(toList(c))

-- Build a map from a list of key/value pairs.
fromList :: Ord k => [(k, a)] -> Map k a
fromList [] = Null
fromList [(k,v)] = (Node Null k v Null)
fromList [(k,v):a] = insert k v (fromList a )

-- The number of elements in the map.
size :: Map k a -> Int
size Null  = 0
size (Node Null k v Null) = 1
size (Node _ k v Null) = 1 + (size x)
size (Node Null k v _) = 1 + (size y)
size (Node x k v y) = 1 + ((size x) + (size y))

-- Is the key a member of the map? 
member :: Ord k => k -> Map k a -> Bool
member _ Null = False
member k (Node Null a b Null) | k == a = True
                              | otherwise = False
member k (Node x a b y) | k == a = True
                        | k > a = (member k x)
                        | k < a = (member k y)

-- Lookup the value at a key in the map.
-- The function will return the corresponding value as (Just value), or Nothing if the key isn't in the map.
lookup :: Ord k => k -> Map k a -> Maybe a
lookup _ Null = Nothing
lookup k (Node Null a b Null) | k == a = Just b
                              | otherwise = Nothing
lookup k (Node x a b y) | k == a = Just b
                        | k > a = (member k x)
                        | k < a = (member k y)

-- Delete a key and its value from the map. When the key is not a member of the map, the original map is returned.
delete :: Ord k => k -> Map k a -> Map k a
delete k Null = Null
delete k (Node Null a b y) | k == a = y 
delete k (Node x a b Null) | k == a = x	
delete k (Node x a b y) | k < x = (delete k left)
                        | k > x = (delete k right)
                        | k == x = let (n,m) = (leftNode (Node x k b y))
                                   in (Node (delete n x) n m y)

leftNode :: Ord k => Map k a -> (k, a)
leftNode (Node x a b Null) = (a, b)
leftNode (Node x a b y) = (leftNode y)


-- The expression (update f k map) updates the value x at k (if it is in the map). If (f x) is Nothing, the element is deleted. 
-- If it is (Just y), the key k is bound to the new value y.
update :: Ord k => (a -> Maybe a) -> k -> Map k a -> Map k a
update f k Null = Null
update f k m | (member k m) == False = m
             | (ifMaybe(f (removeMaybe (lookup k m)))) == True = (delete k m)
             | otherwise = (insert k (removeMaybe (f (removeMaybe (lookup k m)))) m)

ifMaybe :: Maybe a -> Bool
ifMaybe Nothing = True
ifMaybe _ = False

removeMaybe :: Maybe a -> a
removeMaybe (Just a) = a

--  The expression (union t1 t2) takes the left-biased union of t1 and t2. It prefers t1 when duplicate keys are encountered, i.e. (union == unionWith const).
union :: Ord k => Map k a -> Map k a -> Map k a
union Null Null = Null
union t1 Null = t1
union Null t2 = t2
union (Node x a b y) t2 = (union x (union y (insert k v t2))) 

-- Filter all values that satisfy the predicate.
filter :: Ord k => (a -> Bool) -> Map k a -> Map k a
filter f Null = Null
filter f (Node Null k v Null) | (f v) == True = (Node Null k v Null)
                              | otherwise = Null
filter f (Node x k v y) | (f v) == True = (Node (filter f x) k v (filter f y))
                        | otherwise = (delete k (Node (filter f x) k v (filter f y)))


instance (Ord k, Eq v) => Eq (Map k v) where
  x == y = undefined

  
instance Functor (Map k) where
-- Map a function over all values in the map.
--  fmap ::  (a -> b) -> Map k a -> Map k b
  fmap f Null = Null
  fmap f (Node a k v c) = Node (fmap f a) k (f v) (fmap f c)
  
  

-- ungraded bonus


-- Is the map empty?
null :: Map k a -> Bool
null = undefined

-- A map with a single element.
singleton :: k -> a -> Map k a
singleton = undefined

-- Return all elements of the map in the ascending order of their keys.
elems :: Map k a -> [a]
elems = undefined

-- Return all keys of the map in ascending order.
keys :: Map k a -> [k]
keys = undefined

-- Difference of two maps. Return elements of the first map not existing in the second map.
difference :: Ord k => Map k a -> Map k b -> Map k a
difference = undefined

-- Update a value at a specific key with the result of the provided function. When the key is not a member of the map, the original map is returned.
adjust :: Ord k => (a -> a) -> k -> Map k a -> Map k a
adjust = undefined

-- The expression (alter f k map) alters the value x at k, or absence thereof. alter can be used to insert, delete, or update a value in a Map. In short : lookup k (alter f k m) = f (lookup k m).
alter :: Ord k => (Maybe a -> Maybe a) -> k -> Map k a -> Map k a
alter = undefined


-- instance (Ord k, Ord v) => Ord (Map k v) where
  

