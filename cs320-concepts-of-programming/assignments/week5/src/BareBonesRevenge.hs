module BareBonesRevenge where


import Prelude hiding (List(..), Pair(..),Maybe(..),Either(..),)

-- Implement the following type class instances over our bare bones data
-- You may add any dependencies you see fit
-- Follow the instructions, your implementations must obey the standerd type class laws.
-- type class laws will explained in lecture on W 2/30 and explored in the analytical part of this homework (if you get stuck here, look at that). 
-- Note that sometimes laws and typing rules completely define the implementation, 
-- if you don't see instructions look up the laws.


data List a = Nil | Cons a (List a) deriving Show

-- lists are equal when they have equal members in the same order
instance Eq a  => Eq (List a) where
	Nil == Nil = True
	Cons a aList == Cons b bList = (a == b) && (aList == bList)
	_ == _ = False


-- lists are ordered lexographicly over elements
instance Ord a => Ord (List a) where
	Nil <= Nil = True
	(Cons a aList) <= (Cons b bList) = (a <= b) && (aList <= bList)
	_ <= _ = False

instance Functor List where
	fmap f Nil = Nil
	fmap f (Cons a aList) = (Cons (f a) (fmap f aList))


data Pair a b = Pair a b deriving Show

-- Pairs are equal when they have equal members in the same order
instance (Eq a, Eq b) => Eq (Pair a b) where
	(Pair a b) == (Pair c d) = (a == c) && (b == d) 
	_ == _ = False

-- Pair are ordered lexographicly over elements
instance (Ord a, Ord b) => Ord (Pair a b) where
	(Pair a b) <= (Pair c d) = (a <= c) && (b <= d) 
	_ <= _ = False


-- note it could be a functor on both type args
-- keep it easy and just define it on 1
instance Functor (Pair a) where
	fmap f (Pair a b) = (Pair a (f b))


data Maybe a = Nothing | Just a deriving Show


-- Maybe are equal if they are both Just the same thing, or both Nothing
instance Eq a => Eq (Maybe a) where
	Nothing == Nothing = True
	Just a == Just b | (a == b) = True
	_ == _ = False

-- "Nothing" is less than Just, otherwise it matches the order of a
instance Ord a => Ord (Maybe a) where
	Nothing <= _ = True
	Just a <= Just b | (a <= b) = True
	_ <= _ = False

instance Functor Maybe where
	fmap f Nothing = Nothing
	fmap f (Just a) = (Just (f a))



data  Either a b  =  Left a | Right b deriving Show


-- Either are equal if they are both Left and equal or both right and equal
instance (Eq a, Eq b) => Eq (Either a b) where
	Left a == Left b = (a == b) 
	Right a == Right b = (a == b) 
	_ == _ = False

-- Left is less than Right, if the constructors are the same it matches the underlieing order of a and b
instance (Ord a, Ord b) => Ord (Either a b) where
	Left a <= Right b = True
	Left a <= Left b = (a <= b) 
	Right a <= Right b = (a <= b)
	_ <= _ = False

-- note it could be a functor on both type args
-- keep it easy and just define it on 1
instance Functor (Either a ) where
	fmap f (Left a) = Left a
	fmap f (Right a) = Right (f a)


-- a simple data used mostly for demonstration purposes

data Identity a = Identity a deriving Show

-- Identity is equal if the inner type is equal
instance Eq a => Eq (Identity a) where
	Identity a == Identity b = (a == b) 
	_ == _ = False

-- Identity has the same ordering as the inner type
instance Ord a => Ord (Identity a) where
	Identity a <= Identity b = (a <= b) 
	_ <= _ = False

instance Functor Identity where
	fmap f (Identity a) = (Identity (f a)) 


-- a simple data used mostly for demonstration purposes

data Trival a = NoA deriving Show

instance Eq a => Eq (Trival a) where
	NoA == NoA = True
	_ == _ = False

instance Ord a => Ord (Trival a) where
	NoA <= NoA = True


instance Functor Trival where
	fmap f NoA = NoA





