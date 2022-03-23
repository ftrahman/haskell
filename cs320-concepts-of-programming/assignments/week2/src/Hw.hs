module Hw where
import Prelude(Show, undefined)

-- HOMEWORK ONE     Due 2/5 by 11:59pm by upload to your repo
--    (with a couple of hours "grace period" before it is considered late)
--    Note: There is also an analytical part to the homework
--    which will be posted on the class web page, and due to be
--    uploaded to Gradescope with the same due date and time, and
--    with the same grace period. 

-- Fill in the bodies of the undefined functions and data.
-- DO NOT CHANGE THE TYPE SIGNATURES!

-- Think about whether you need to write each function
-- recursively on the structure of the data, or can
-- define more simply in terms of previously-defined functions,
-- or use a helper function. 
-- You may always add your own helper functions and helper data!

-- Remember: Constructors must be capitalized; variable
-- and function names must be in lower case. 
-- Constructor constants are like 0-ary functions (no arguments). 


-- Part A: Basic Boolean data and functions

-- Note on data declarations: "deriving Show" will allow 
-- data values to be printed by interpreter.


data Bool = True | False     deriving Show

-- Define the following familiar functions on Bools.
-- You may need multiple cases for each one.

not :: Bool -> Bool
not True = False
not False = True


and :: Bool -> Bool -> Bool
and True True = True
and _ _ = False


or :: Bool -> Bool -> Bool
or False False = False
or _ _ = True


xor :: Bool -> Bool -> Bool
xor True False  = True
xor False True = True
xor _ _ = False


-- Part B: Encoding of natural numbers using data expressions
-- and defining basic functions on these expressions.

data Nat =  Zero | Succ Nat deriving Show

-- the first 6 numbers
zero :: Nat
zero = Zero

one :: Nat
one = Succ zero

two :: Nat
two = Succ one

three  :: Nat
three = Succ two

four  :: Nat
four = Succ three

five  :: Nat
five = Succ four


-- Write the following functions
-- (Hint: try recursing on structure of first argument)

add ::  Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = Succ (add n m)

mult ::  Nat -> Nat -> Nat
mult Zero n  = Zero
mult (Succ n) m = add m (mult n m)

exp ::  Nat -> Nat -> Nat
exp Zero n = (Succ Zero)
exp (Succ Zero) n = n 
exp (Succ n) m = mult m (exp n m)

-- When are 2 Nats equal? 
eq :: Nat -> Nat -> Bool
eq Zero Zero  = True
eq Zero (Succ n) = False
eq (Succ n) Zero = False
eq (Succ n) (Succ m) = eq n m


-- When are 2 Nats not equal?  
ne :: Nat -> Nat -> Bool
ne Zero Zero = False
ne Zero (Succ n) = True
ne (Succ n) Zero = True
ne (Succ n) (Succ m) = ne n m


-- Less than on Nats
lt :: Nat -> Nat -> Bool
lt Zero Zero  = False
lt Zero (Succ n) = True
lt (Succ n) Zero = False
lt (Succ n) (Succ m) = lt n m

-- Remaining Boolean tests

le :: Nat -> Nat -> Bool
le Zero Zero = True
le Zero (Succ n) = True
le (Succ n) Zero = False
le (Succ n) (Succ m) = le n m 

gt :: Nat -> Nat -> Bool
gt Zero Zero = False
gt Zero (Succ n) = False
gt (Succ n) Zero = True
gt (Succ n) (Succ m) = gt n m 

ge :: Nat -> Nat -> Bool
ge Zero Zero = True
ge Zero (Succ n) = False
ge (Succ n) Zero = True
ge (Succ n) (Succ m) = ge n m 

-- Example of useful test on Nats
-- return True on even Nats, False on odd.
isEven :: Nat -> Bool
isEven Zero = True
isEven (Succ Zero) = False
isEven (Succ(Succ n)) = isEven n

--Return the maximum of two Nats
max :: Nat -> Nat -> Nat
max n Zero = n 
max Zero n = n
max (Succ n) (Succ m) = Succ(max n m)


-- Part C:  Data Expressions: Now let's write our own data.

-- C.1: Dates

-- Write a data type for the 7 days of the week
data DayOfWeek = Mon|Tues|Wed|Thurs|Fri|Sat|Sun deriving Show

-- what is your favorite Day? (Your choice!)
favoriteDay :: DayOfWeek
favoriteDay = Fri

-- write a function that returns true if it is a weekend
isWeekend :: DayOfWeek -> Bool
isWeekend Sat = True
isWeekend Sun = True
isWeekend _ = False

-- Write a function that gives the next day
nextDay :: DayOfWeek -> DayOfWeek
nextDay Mon = Tues
nextDay Tues = Wed
nextDay Wed = Thurs
nextDay Thurs = Fri
nextDay Fri = Sat
nextDay Sat = Sun
nextDay Sun = Mon

-- write a data type for the Months of the year
data Month = Jan|Feb|Mar|Apr|May|June|July|Aug|Sep|Oct|Nov|Dec  deriving Show

-- In which month is your birthday?
partyMonth :: Month
partyMonth = Feb


-- Write a function that gives the next Month
nextMonth :: Month -> Month
nextMonth Jan = Feb
nextMonth Feb = Mar
nextMonth Mar = Apr
nextMonth Apr = May
nextMonth May = June
nextMonth June = July
nextMonth July = Aug
nextMonth Aug = Sep
nextMonth Sep = Oct
nextMonth Oct = Nov
nextMonth Nov = Dec
nextMonth Dec = Jan


-- C.2: Cartesian Coordinates

-- Write a data type for a 2D point where x and y are Nats
data Point = Pair Nat Nat deriving Show

-- Take 2 Nats and construct a Point
makePoint :: Nat -> Nat -> Point
makePoint x y = Pair x y 

-- Select components from a Point
getX :: Point -> Nat
getX (Pair x y) = x

getY :: Point -> Nat
getY (Pair x y) = y


-- The Manhattan distance is the distance in the x direction plus the distance in the y direction
-- for instance the Manhattan distance of points (2,5) and (3,1) is 5

absMinus:: Nat -> Nat -> Nat
absMinus Zero Zero = Zero
absMinus Zero (Succ m) = Succ m
absMinus (Succ n) Zero = Succ n
absMinus (Succ n) (Succ m) = absMinus n m 

manhattanDistance :: Point -> Point -> Nat
manhattanDistance (Pair a b) (Pair c d) = (add(absMinus (getX(Pair a b)) (getX(Pair c d))) (absMinus(getY(Pair a b)) (getY(Pair c d))))



-- C.3: More Alterative data

-- Assume there is an boring math class where students only answer with a Bool OR with a Nat, 
-- write a data type for that answer (hint: you may use two alternatives with | ) 
data ShortAnswer = BoolAnswer Bool|NatAnswer Nat  deriving Show

-- Make a Nat answer
answerNat :: Nat -> ShortAnswer
answerNat x = NatAnswer x

-- Make a Bool answer
answerBool :: Bool -> ShortAnswer
answerBool y = BoolAnswer y

-- What is 100 - 99?
ans1 :: ShortAnswer
ans1 = NatAnswer(Succ Zero)

-- Is 100 - 99 an odd number?
ansTrue :: ShortAnswer
ansTrue = BoolAnswer(not(isEven(Succ Zero)))

-- If the answers are equal return true otherwise return false
gradeAnswer :: ShortAnswer -> ShortAnswer -> Bool
gradeAnswer (NatAnswer x) (NatAnswer y) = eq x y
gradeAnswer (BoolAnswer x) (BoolAnswer y) = and x y
gradeAnswer _ _ = False

-- Part D: Important data structures: Lists

-- D.1: Lists of Nats

-- We can write lists for specific data, let's do Nats first
data ListNat = NilNat | ConsNat Nat ListNat    deriving Show

-- Create a list of the first 4 nats
exampleListNat :: ListNat
exampleListNat = ConsNat three(ConsNat two (ConsNat one (ConsNat Zero(NilNat))))

-- Find the length of a list (remember length is defined as the number of elements in the list)
lengthOfListNat :: ListNat -> Nat
lengthOfListNat NilNat = Zero
lengthOfListNat (ConsNat n nList) = Succ(lengthOfListNat nList)

-- Write a function that finds the sum of all the numbers in the list
sum :: ListNat -> Nat
sum NilNat = Zero
sum (ConsNat n nList)= (add n (sum (nList)))

-- Write a function that tells when 2 Nat lists are equal
eqList :: ListNat -> ListNat -> Bool
eqList NilNat NilNat  = True
eqList (ConsNat n nList) NilNat = False
eqList NilNat (ConsNat n nList) = False 
eqList (ConsNat n nList) (ConsNat m mList) = (and (eq n m) (eqList nList mList))

-- Write a function that tests when a Nat is in a list
member :: Nat -> ListNat -> Bool
member _ NilNat= False
member n (ConsNat m mList) = (or (eq n m) (member n mList))


-- D.2:  Now let's do lists of Bools

data ListBool = NilBool | ConsBool Bool ListBool deriving Show

-- Give a list containing every bool
exampleListBool :: ListBool
exampleListBool = ConsBool True(ConsBool False (NilBool))


lengthOfListBool :: ListBool -> Nat
lengthOfListBool NilBool = Zero
lengthOfListBool (ConsBool n nList) = Succ(lengthOfListBool nList)


-- D.3:  General lists: It gets very tiresome to write a list for every single datatype
-- so let's abstract out the type of elements using a polymorphic type

data List a = Nil | Cons a (List a)    deriving Show

-- Write a list of all the Bool values
listOfBool :: (List Bool)
listOfBool = Cons True(Cons False(Nil))

-- Write a list of the first three Nats
listOfNat :: (List Nat)
listOfNat = Cons two(Cons one(Cons Zero(Nil)))

-- Write a list of all the weekdays
listOfWork :: (List DayOfWeek)
listOfWork = Cons Mon(Cons Tues(Cons Wed(Cons Thurs(Cons Fri(Cons Sat(Cons Sun(Nil)))))))

-- Useful function on lists
length :: (List a) -> Nat
length Nil = Zero
length (Cons n nList) = Succ(length nList)

-- Part E: Binary trees

-- A binary tree is either empty, or a node with a left subtree
-- a value at the root and a right subtree
data Tree a = Null | Node (Tree a) a (Tree a)     deriving Show

-- Give a balanced tree of three Bools corresponding to
--             True
--            /    \
--        False    False

exampleTree :: Tree Bool
exampleTree = Node (Node Null False Null) True (Node Null False Null)

-- return the number of elements in the tree
size :: (Tree a) -> Nat
size Null  = Zero
size (Node x _ Null) = Succ(size x)
size (Node Null _ y) = Succ(size y)
size (Node x _ y) = Succ(add(size x) (size y))

-- Return the height (= number of nodes in longest path from root to leaf)

height :: (Tree a) -> Nat
height Null  = Zero
height (Node x _ y) = Succ(max(height x) (height y))


append :: (List a) -> (List a) -> (List a)
append Nil Nil  = Nil
append (Cons a aList) Nil = (Cons a aList)
append Nil (Cons b bList) = (Cons b bList)
append (Cons a aList) (Cons b bList) = Cons a (append aList(Cons b bList))


-- Do an inorder traversal and store elements in a list
inorder :: (Tree a) -> (List a)
inorder Null = Nil
inorder (Node Null y Null) = (Cons y Nil)
inorder (Node x y z) =(append (inorder x) (Cons y (inorder z)))

-- Do a preorder traversal
preorder :: (Tree a) -> (List a)
preorder Null = Nil
preorder (Node Null y Null) = (Cons y Nil)
preorder (Node x y z) =(append (Cons y (preorder x)) (preorder z))


-- extra ungraded questions below

-- What is the smallest datatype you can come up with?
data Smallests -- = ... deriving Show

exampleSmallest = undefined

-- what is the craziest datatype you can come up with?
data Craziests -- = ... deriving Show

exampleCraziests = undefined
