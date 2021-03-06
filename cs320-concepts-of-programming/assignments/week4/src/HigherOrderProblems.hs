module HigherOrderProblems where

import Map (Map)
import qualified Map as Map
import Prelude hiding (zipWith, sum)
import Data.List hiding (zipWith, sum)
import Data.Function



-- add 1 to each element in a list
addoneList :: [Integer] -> [Integer]
addoneList [] = []
addoneList (x:xs) = ((1+x):addoneList xs)

-- add 1 to each element in a Map
-- (you will need to finish the Map part of the HW first, hint: use fmap)
addoneMap :: Ord k => Map k Integer -> Map k Integer
addoneMap = undefined

-- keep only the elements of the list greater than 2
keepGreaterThan2List :: [Integer] -> [Integer]
keepGreaterThan2List [] = []
keepGreaterThan2List (x:xs) = filter (>2) (x:xs)

-- keep only the elements of the Map greater than 2
-- (you will need to finish the Map part of the HW first)
keepGreaterThan2Map :: Ord k => Map k Integer -> Map k Integer
keepGreaterThan2Map = undefined
--keepGreaterThan2Map Null = Null
--keepGreaterThan2Map m = (filter (\x -> (x>2)) m)


-- define sum with foldr
sum :: [Integer] -> Integer
sum xs = foldr (+) 0 xs

-- define product with foldr
product :: [Integer] -> Integer
product xs = foldr (*) 1 xs

-- Write maxList, which returns the largest element
-- in a list of positive numbers; it should return 0
-- for the a list with no positive numbers.
-- Hint: try using the built-in function max.
maxList :: (Ord a, Num a) => [a] -> a
maxList lst = foldr (max) 0 lst


-- zipWith generalises zip by zipping with the function given as the first argument,
-- instead of a tupling function. For example, zipWith (+) is applied to two
-- lists to produce the list of corresponding sums.
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- dot product from linear algebra (on integers only)
-- Hint: try using zipWith and sum
dot :: [Integer] -> [Integer] -> Integer
dot [] [] = 0
dot xs ys = sum (zipWith (*) xs ys)

-- combines two lists by adding their corresponding elements using zipWith
-- Ex: listAdder [1,2,3] [2,3,4] => [3,5,7]
listAdder :: [Integer] -> [Integer] -> [Integer]
listAdder xs ys = (zipWith (+) xs ys)

-- Find the maximum of each pair of integers in two lists using zipWith
-- Ex:  listMaxer [2,4] [4,1] => [4,4]
listMaxer :: [Integer] -> [Integer] -> [Integer]
listMaxer xs ys = zipWith (max) xs ys
-- We have first names and last names, how to
-- put them together with a space in the middle?
-- Should work for lists of any size.

firstNames = ["Cheng", "Mark", "Wayne"]
lastNames = ["Zhang", "Lemay", "Snyder"]

-- Ex: fullNames firstNames lastNames => ["Cheng Zhang", ....]

fullNames :: [String] -> [String] -> [String]
fullNames fns lns = zipWith (++) (namesHelper fns) lns

namesHelper :: [String] -> [String]
namesHelper [] = []
namesHelper (x:xs) = (x ++ " "):(namesHelper xs)
-- This function extends the idea of listAdder to an arbitrary number
-- of lists: given a list of lists, add all the first elements,
-- all the second elements, and so on.
-- Hint: you may use zipWith recursively!

-- Ex:  addAllLists [ [2,3,4], [4,5,3], [1,1,1], [3,5, 0]] =>  [10, 14,8]
addAllLists :: [[Integer]] -> [Integer]
addAllLists (x:xs) = map sum $ transpose (x:xs)


-- define a data type for pets, all pets have names (String)
-- Cats are happy depending on the current temperature (Integer)
-- Dogs are always happy
-- Hint: you may need to store a function in the constructor!
-- if you store a function in the constructor you cannot "deriving Show"
data Pet = Pet String | Cat String (Integer -> Bool) | Dog String Bool

-- define a cat that is happy when the temperature is > 0
okCat :: Pet
okCat =  (Cat "name" (>0))

-- define a cat that is never happy
badCat :: Pet
badCat = (Cat "name" (\x -> False))

-- make a cat from a name and the function that tells how it will be happy
mkCat :: String -> (Integer -> Bool) -> Pet
mkCat s f = (Cat s (\x -> f(x)))

-- make a dog from a name.
mkDog :: String -> Pet
mkDog s = Dog s True

-- some functions to help out the graders
name :: Pet -> String
name (Pet n) = n
name (Cat n x) = n
name (Dog n x) = n

isCat :: Pet -> Bool
isCat (Pet _) = False
isCat (Cat _ _) = True
isCat (Dog _ _) = False

isDog :: Pet -> Bool
isDog (Pet _) = False
isDog (Cat _ _) = False
isDog (Dog _ _) = True

-- is a pet happy given the current temperature (Integer)
isHappy :: Pet -> Integer -> Bool
isHappy (Dog _ _) t = True
isHappy (Cat n h) t = (\x -> h(x))(t)

-- How many of your pets are happy?
-- takes in a list of your pets and the current temperature
-- Hint: try to do this in a higher-order way:
--       use map, filter, and length,
--       OR two maps and sum,
--       OR foldr!
countHappy :: [Pet] -> Integer -> Integer
countHappy [] x = 0
countHappy [(Dog _ _)] t = 1
countHappy (Dog _ _:op) t  = (1 + (countHappy op t))
countHappy [(Cat x y)] t | (isHappy (Cat x y) t) = 1
                         | otherwise = 0
countHappy (Cat x y:op) t | (isHappy (Cat x y) t) = (1 + (countHappy op t))
                            | otherwise = (countHappy op t)


-- Higher order programming works well for data exploration

-- record of: year, team name, Won, Lost, Ties
data FootBallStat = FootBallStat Integer String Integer Integer Integer deriving Show

footballExampleStats :: [FootBallStat]
footballExampleStats = [FootBallStat 1960 "Patriots" 5 9 0, FootBallStat 1961 "Patriots" 9 4 1, FootBallStat 1962 "Patriots" 9 4 1, FootBallStat 1963 "Patriots" 7 6 1, FootBallStat 1964 "Patriots" 10 3 1, FootBallStat 1965 "Patriots" 4 8 2]--, FootBallStat 1966 "Patriots" 8 4 2, FootBallStat 1967 "Patriots" 3 10 1, FootBallStat 1968 "Patriots" 4 10 0, FootBallStat 1969 "Patriots" 4 10 0, FootBallStat 1970 "Patriots" 2 12 0, FootBallStat 1971 "Patriots" 6 8 0, FootBallStat 1972 "Patriots" 3 11 0, FootBallStat 1973 "Patriots" 5 9 0, FootBallStat 1974 "Patriots" 7 7 0, FootBallStat 1975 "Patriots" 3 11 0, FootBallStat 1976 "Patriots" 11 3 0, FootBallStat 1977 "Patriots" 9 5 0, FootBallStat 1978 "Patriots" 11 5 0, FootBallStat 1979 "Patriots" 9 7 0, FootBallStat 1980 "Patriots" 10 6 0, FootBallStat 1981 "Patriots" 2 14 0, FootBallStat 1982 "Patriots" 5 4 0, FootBallStat 1983 "Patriots" 8 8 0, FootBallStat 1984 "Patriots" 9 7 0, FootBallStat 1985 "Patriots" 11 5 0, FootBallStat 1986 "Patriots" 11 5 0, FootBallStat 1987 "Patriots" 8 7 0, FootBallStat 1988 "Patriots" 9 7 0, FootBallStat 1989 "Patriots" 5 11 0, FootBallStat 1990 "Patriots" 1 15 0, FootBallStat 1991 "Patriots" 6 10 0, FootBallStat 1992 "Patriots" 2 14 0, FootBallStat 1993 "Patriots" 5 11 0, FootBallStat 1994 "Patriots" 10 6 0, FootBallStat 1995 "Patriots" 6 10 0, FootBallStat 1996 "Patriots" 11 5 0, FootBallStat 1997 "Patriots" 10 6 0, FootBallStat 1998 "Patriots" 9 7 0, FootBallStat 1999 "Patriots" 8 8 0, FootBallStat 2000 "Patriots" 5 11 0, FootBallStat 2001 "Patriots" 11 5 0, FootBallStat 2002 "Patriots" 9 7 0, FootBallStat 2003 "Patriots" 14 2 0, FootBallStat 2004 "Patriots" 14 2 0, FootBallStat 2005 "Patriots" 10 6 0, FootBallStat 2006 "Patriots" 12 4 0, FootBallStat 2007 "Patriots" 16 0 0, FootBallStat 2008 "Patriots" 11 5 0, FootBallStat 2009 "Patriots" 10 6 0, FootBallStat 2010 "Patriots" 14 2 0, FootBallStat 2011 "Patriots" 13 3 0, FootBallStat 2012 "Patriots" 12 4 0, FootBallStat 2013 "Patriots" 12 4 0, FootBallStat 2014 "Patriots" 12 4 0, FootBallStat 2015 "Patriots" 12 4 0, FootBallStat 2016 "Patriots" 14 2 0, FootBallStat 2017 "Patriots" 13 3 0, FootBallStat 2018 "Patriots" 11 5 0]

-- hint: the following functions are easy if you use map, filter, and pattern matching in a lambda

-- map: returns a list constructed by appling a function (the first argument) to all items in a list passed as the second argument
-- filter: returns a list constructed from members of a list (the second argument) fulfilling a condition given by the first argument


yearAndTeamWithTotalGames  :: [FootBallStat] -> [(Integer, String, Integer)]
yearAndTeamWithTotalGames [] = []
yearAndTeamWithTotalGames [(FootBallStat y n w l t)] = [(y, n, (w + t + l))]
yearAndTeamWithTotalGames ((FootBallStat y n w l t):fbs) = [(y, n, (w + t + l))] ++ (yearAndTeamWithTotalGames fbs)
--yearAndTeamWithTotalGames [(FootBallStat y n w t l), fbs] = [(y, n, (w + t + l))] ++ (yearAndTeamWithTotalGames [fbs])

yearAndTeamWithMoreThan1Tie  :: [FootBallStat] -> [(Integer, String)]
yearAndTeamWithMoreThan1Tie [] = []
yearAndTeamWithMoreThan1Tie [(FootBallStat y n w l t)] | (t > 1) = [(y, n)]
                                                       | otherwise = []
yearAndTeamWithMoreThan1Tie ((FootBallStat y n w l t):fbs) | (t > 1) = [(y, n)] ++ (yearAndTeamWithMoreThan1Tie fbs)
                                                            | otherwise = (yearAndTeamWithMoreThan1Tie fbs)


yearAndTeamWithlessThan3Wins  :: [FootBallStat] -> [(Integer, String)]
yearAndTeamWithlessThan3Wins [] = []
yearAndTeamWithlessThan3Wins [(FootBallStat y n w l t)] | (w < 3) = [(y, n)]
                                                        | otherwise = []
yearAndTeamWithlessThan3Wins ((FootBallStat y n w l t):fbs) | (w < 3) = [(y, n)] ++ (yearAndTeamWithlessThan3Wins fbs)
                                                             | otherwise = (yearAndTeamWithlessThan3Wins fbs)

yearsAndTeamWithMoreWinsThanLosses :: [FootBallStat] -> [(Integer, String)]
yearsAndTeamWithMoreWinsThanLosses [] = []
yearsAndTeamWithMoreWinsThanLosses [(FootBallStat y n w l t)] | (w > l) = [(y, n)]
                                                              | otherwise = []
yearsAndTeamWithMoreWinsThanLosses ((FootBallStat y n w l t):fbs) | (w > l) = [(y, n)] ++ (yearsAndTeamWithMoreWinsThanLosses fbs)
                                                                   | otherwise = (yearsAndTeamWithMoreWinsThanLosses fbs)


-- ungraded bonus:

wins :: FootBallStat -> Integer
wins = undefined

-- find the record with the most wins each decade
-- Hint you may use (maximumBy (compare `on` wins)) to find the best win record in a list
-- you can use groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
mostWinsEachDecade :: [FootBallStat] -> [FootBallStat]
mostWinsEachDecade xs = undefined
