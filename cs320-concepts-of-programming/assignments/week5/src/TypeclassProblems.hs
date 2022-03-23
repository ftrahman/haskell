module TypeclassProblems where

-- define data type for all 7 days of the week
data DayOfTheWeek = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show

-- often built-in haskell classes come with nice syntax, when the following is defined try this 
-- in the console [Monday .. Friday].  

-- Note: do NOT uncomment the types for these functions,
-- they are given in the class declaration, so all you need to do is give the definition. The type is
-- just for reference. 

instance Enum DayOfTheWeek where
  --toEnum :: Integer -> DayOfTheWeek (start counting at 0)
  toEnum 0 = Monday
  toEnum 1 = Tuesday
  toEnum 2 = Wednesday
  toEnum 3 = Thursday
  toEnum 4 = Friday
  toEnum 5 = Saturday
  toEnum 6 = Sunday
  
  --fromEnum :: DayOfTheWeek -> Integer
  fromEnum Monday = 0
  fromEnum Tuesday = 1
  fromEnum Wednesday = 2
  fromEnum Thursday = 3
  fromEnum Friday = 4
  fromEnum Saturday = 5
  fromEnum Sunday = 6


-- First we will work with a custom type class that makes an example of a type
class HasExample a where
  example :: a

-- finish up the instances on these types
instance HasExample DayOfTheWeek where
  example = Monday


instance HasExample Bool where
  example = True
  
instance HasExample Integer where
  example = 5

instance HasExample [a] where
  example = []
  
instance (HasExample a, HasExample b) => HasExample (a,b) where
  example = (example, example)

instance HasExample b => HasExample (a -> b) where
  example = (\x -> example)
  
iSureWishIHadThisFunction :: Integer -> Bool -> (a ->b ) -> (Integer, (Bool, DayOfTheWeek))
iSureWishIHadThisFunction = example -- it's a little silly but the code is automatically generated!


-- next we will work with a custom type class that gives all the things

class AllTheThings a where
  listOfAll :: [a]
-- laws: finite, no repeat

-- when we have this defined we can check that ALL inputs of a function are correct
forAll :: AllTheThings a => (a -> Bool) -> Bool
forAll f = all f listOfAll


instance AllTheThings Bool where
  listOfAll = [True, False]

  
boolEq  :: Bool -> Bool
boolEq = (\b  -> b == b)

--try "forAll boolEq" in the console!

-- finish up the instances on these types
instance AllTheThings DayOfTheWeek where
  listOfAll = [Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday]
  

allTuples :: [a] -> [b] -> [(a,b)]
allTuples [] [] = []
allTuples [] _ = []
allTuples _ [] = []
allTuples (a:aList) (b:bList) = (stickInFront a (b:bList)) ++ (allTuples aList (b:bList))


stickInFront :: a -> [b] -> [(a,b)]
stickInFront x [] = []
stickInFront a (b:bList) = [(a,b)] ++ (stickInFront a bList)

instance (AllTheThings a, AllTheThings b) => AllTheThings (a,b) where
  listOfAll = (allTuples listOfAll listOfAll)




  
-- Ungraded bonus challenge problems!
  
instance (AllTheThings a, Eq a, AllTheThings b) => AllTheThings (a -> b) where
  listOfAll = undefined

instance (AllTheThings a, Show a, Show b) => Show (a -> b) where
  show f = undefined
