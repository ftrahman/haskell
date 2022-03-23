module Hw02 where

import Prelude (Show,undefined,Bool(True,False),(&&),(||),not,
                Integer,(==),(/=),(<),(>),(<=),(>=),(+),(-),(*),div,mod)


        
--COLLABORATED WITH NATHAN LEVY AND MCKENZIE JOYCE        
-- Part A: Practice defining functions on Integers
--         using if-then-else, == on integers, guards,
--         and where clauses to hide helper functions,
--         plus have all normal functions on Integers. 

-- fibonnaci
-- example: fib 0 = 0
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib(x-2)

-- Greatest Common Divisor

gcd :: Integer -> Integer -> Integer
gcd x 0 = x
gcd 0 x = x
gcd x y = gcd y (mod x y)

otherwise :: Bool
otherwise = True


-- Part B: Practice functions on lists


data List a = Nil | Cons a (List a) deriving Show

isEmpty :: List a -> Bool
isEmpty Nil = True
isEmpty _  = False

length :: List a -> Integer
length Nil  = 0
length (Cons n nList) = 1 + (length nList)

-- Concatenate two lists, make sure it works
-- if either or both are empty
-- Hint: structural induction on first argument
-- you may have done this in the last hw
(++) :: (List a) -> (List a) -> (List a)
(++) Nil Nil  = Nil
(++) (Cons a aList) Nil = (Cons a aList)
(++) Nil (Cons b bList) = (Cons b bList)
(++) (Cons a aList) (Cons b bList) = Cons a (aList ++(Cons b bList))

-- Add an element to the end of the list
addToEnd :: a -> (List a) -> (List a)
addToEnd n Nil = (Cons n Nil)
addToEnd n (Cons m mList) = (++) (Cons m mList) (Cons n Nil)


-- reverse a list
reverse :: List a -> List a
reverse Nil = Nil
reverse (Cons n nList) = ((++) (reverse nList)(Cons n Nil))

-- flatten a list of lists
concat :: List (List a) -> List a
concat Nil = Nil
concat (Cons Nil nList) = concat nList
concat (Cons n nList) = (++) n (concat nList)

-- return the first n elements
take :: Integer -> List a -> List a
take 0 _ = Nil
take n Nil = Nil
take n (Cons m mList) | n == 1 = (Cons m Nil)
take n (Cons m mList) = (++) (Cons m Nil)(take (n-1) mList)


-- some "higher order" functions

-- apply the function to each member of the list
map :: (a -> b) ->  List a ->  List b
map f Nil = Nil
map f (Cons n nList) = (Cons (f n)(map f nList))

multiplyEachBy7 :: List Integer -> List Integer
multiplyEachBy7 Nil= Nil
multiplyEachBy7 (Cons n nList) = (Cons (n * 7) (multiplyEachBy7 (nList)))


-- keep only the elements from the list that have the property of interest
filter :: (a -> Bool) ->  List a ->  List a
filter n Nil = Nil
filter n (Cons m mList)  | n m = (Cons m (filter n mList))
                         | otherwise = (filter n mList)

keepEvens :: List Integer ->  List Integer
keepEvens Nil = Nil
keepEvens (Cons n nList) | (n `mod` 2) == 0 = (Cons n (keepEvens nList))
                         | otherwise = (keepEvens nList) 

-- Pairs

data Pair a b = Pair a b deriving Show


fst :: Pair a b -> a
fst (Pair n _) = n

snd :: Pair a b -> b
snd (Pair _ n) = n

-- zip 2 lists together
zip :: (List a) -> (List b) -> List (Pair a b)
zip Nil Nil = Nil
zip (Cons n nList) Nil = Nil
zip Nil (Cons n nList) = Nil
zip (Cons n nList) (Cons m mList) = (++) (Cons (Pair n m) Nil) (zip nList mList)



-- Maybe

-- sometimes we don't know if we will have a result.  So we can use the "Maybe" datatype.

data Maybe a = Nothing | Just a deriving Show

head :: List a -> Maybe a
head Nil = Nothing
head (Cons n _) = Just n


last :: List a -> Maybe a
last Nil = Nothing
last (Cons n Nil) = Just n
last (Cons n nList) = last nList


-- Next we define an infix function

-- return nth element in list, starting at 0; similar to array indexing
(!!) :: List a -> Integer -> Maybe a
(!!) (Cons n _) 0 = Just n
(!!) (Cons n nList) m = nList !! (m-1)


-- sorting 

data Comparison = LessThan | EqualTo | GreaterThan deriving Show

compareInteger :: Integer -> Integer -> Comparison
compareInteger n m | n > m = GreaterThan
compareInteger n m | n < m = LessThan
compareInteger n m = EqualTo

-- when false < true
compareBool :: Bool -> Bool -> Comparison
compareBool True True = EqualTo
compareBool False False = EqualTo
compareBool True False = GreaterThan
compareBool False True = LessThan

-- when we have frequently used functions, we can wrap them in data
data Ordering a = Ordering (a -> a -> Comparison)

intOrd :: Ordering Integer
intOrd = Ordering compareInteger

boolOrd :: Ordering Bool
boolOrd = Ordering compareBool

-- let's write insertion sort

-- inserts an a into a sorted list of a (the list is sorted least to greatest)
-- for example: insert intOrd 3 (Cons 1 (Cons 4 Nil)) = (Cons 1 (Cons 3 (Cons 4 Nil)))
insert :: Ordering a -> a -> List a -> List a
insert (Ordering _ ) n Nil = (Cons n Nil)
insert (Ordering (compareInteger)) n Nil = (Cons n Nil)
insert (Ordering (compareBool)) n Nil = (Cons n Nil)
insert (Ordering compareInteger) n (Cons m mList) = case (compareInteger n m) of GreaterThan -> (Cons m (Cons n mList))
                                                                                 LessThan -> (insert (Ordering compareInteger) n mList)
                                                                                 EqualTo -> (Cons n (Cons m mList))
insert (Ordering compareBool) n (Cons m mList) = case (compareBool n m) of GreaterThan -> (Cons m (Cons n mList))
                                                                           LessThan -> (insert (Ordering compareBool) n mList)
                                                                           EqualTo -> (Cons n (Cons m mList))


-- sort the list
sort :: Ordering a -> List a -> List a
sort intOrd Nil = Nil
sort intOrd (Cons n Nil) = (Cons n Nil)
sort intOrd (Cons n nList) = insert intOrd n (sort intOrd nList)


-- write a datatype representing a student, the student should have a bu-ID, current year
-- CS students should have a Bool (are they taking 320)
-- Math students have an Integer  (how many friends do they have)

data Student = Student Integer Integer | CsStudent Integer Integer Bool | MathStudent Integer Integer Integer  deriving Show

-- from buid, year, and if they are taking 320
-- for instance a freshman in this class would be created by, "mkCsStudent 12345678 0 True"
mkCsStudent :: Integer ->  Integer ->  Bool -> Student
mkCsStudent a b c = CsStudent a b c


-- from buid, year, and how many friends they have
mkMathStudent :: Integer ->  Integer ->  Integer -> Student
mkMathStudent a b c = MathStudent a b c

-- is this a CS student? (this is to make testing easier)
isCs :: Student ->  Bool
isCs (CsStudent a b c) = True
isCs _ = False

isMath :: Student ->  Bool
isMath (MathStudent a b c) = True
isMath _ = False

getBuId :: Student ->  Integer
getBuId (Student a b) = a
getBuId (CsStudent a b c) = a
getBuId (MathStudent a b c) = a

getYear :: Student ->  Integer
getYear (Student a b) = b
getYear (CsStudent a b c) = b
getYear (MathStudent a b c) = b

-- coolestStudent is defined to be the first CS student who is taking 320 in this list
coolestStudent :: List Student -> Maybe Student
coolestStudent Nil = Nothing
coolestStudent (Cons (Student a b) Nil) = Nothing
coolestStudent (Cons (MathStudent a b c) Nil) = Nothing
coolestStudent (Cons (CsStudent a b c) Nil) | c == True = Just (CsStudent a b c)
                                            | otherwise = Nothing
coolestStudent (Cons (Student _ _ ) Nil) = Nothing
coolestStudent (Cons (MathStudent a b c) aList) = (coolestStudent aList)
coolestStudent (Cons (CsStudent a b c) aList)| c == True = Just (CsStudent a b c)
                                             | otherwise = (coolestStudent aList)


-- the students need to be put into pairs for group projects.
-- Take the list of students and create pairs of math and CS students from the list.
-- If possible every Math student should be paired with a CS student.
-- If some students are not paired up that is ok.
groupProject :: List Student -> List (Pair Student Student)
groupProject Nil = Nil
groupProject (Cons n Nil) = Nil
groupProject (Cons n nList) = (zip (filter isCs (Cons n nList)) (filter isMath (Cons n nList)))

studentOrd :: Ordering Student
studentOrd = undefined


