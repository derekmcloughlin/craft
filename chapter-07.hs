import Pictures
import Test.QuickCheck
import Test.HUnit
import Data.Char
import Data.List

-- Exercise 7.1

firstPlusOne :: [Integer] -> Integer

firstPlusOne (x:xs) = x + 1
firstPlusOne [] = 0

-- Exercise 7.2

addFirstTwo :: [Integer] -> Integer

addFirstTwo (x1:x2:xs) = x1 + x2
addFirstTwo (x1:xs) = x1 
addFirstTwo [] = 0

-- Exercise 7.3

firstPlusOne' :: [Integer] -> Integer

firstPlusOne' xs
    | length xs >= 1 = (head xs) + 1
    | otherwise = 0

addFirstTwo' :: [Integer] -> Integer

addFirstTwo' xs
    | length xs >= 2 = (head xs) + (head $ tail xs)
    | length xs == 1 = head xs
    | otherwise = 0

-- Exercise 7.4

firstDigit' :: String -> Char

firstDigit' st = head [c | c <-st, isDigit c]

-- Exercise 7.5

product' :: [Integer] -> Integer

product' (x:xs) = x * product xs
product' [] = 1

-- Exercise 7.6

and' :: [Bool] -> Bool
or' :: [Bool] -> Bool

and' (x:xs) = x && and' xs
and' [] = True

or' (x:xs) = x || or' xs
or' [] = False


-- Exercise 7.7

prop_and xs = and' xs == and xs

prop_or xs = or' xs == or xs


-- Exercise 7.8

elemNum :: Integer -> [Integer] -> Integer

elemNum x xs
    | xs == [] = 0
    | x == head xs = 1 + elemNum x (tail xs)
    | otherwise = 0 + elemNum x (tail xs)

-- Non-recursive
elemNum' :: Integer -> [Integer] -> Integer

elemNum' x xs = toInteger $ length [y | y <- xs, y == x]


-- Exercise 7.9

--unique' :: [Integer] -> [Integer]

--unique' xs = [y | x <- xs, 


-- List Comprehensions
--unique'' :: [Integer] -> [Integer]

--unique'' xs = [y | x <- xs, 



-- Exercise 7.10




-- Exercise 7.11

reverse' :: [a] -> [a]

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

unzip' :: [(a, b)] -> ([a], [b])

unzip' [] = ([], [])
unzip' ((x, y):xs) = ([x] ++ fst unzippedRest, [y] ++ snd unzippedRest)
    where unzippedRest = unzip' xs

-- Exercise 7.12

iSort :: [Integer] -> [Integer]

iSort [] = []
iSort (x: xs) = ins x (iSort xs)

ins :: Integer -> [Integer] -> [Integer]
ins x [] = [x]
ins x (y:ys)
    | x <= y    = x:(y:ys)
    | otherwise = y : ins x ys


max' :: [Integer] -> Integer
max' xs = last $ iSort xs

min' :: [Integer] -> Integer
min' xs = head $ iSort xs

-- without sorting?

max'' :: [Integer] -> Integer
max'' [] = 0   -- not great - asumes positive integers
max'' [x] = x
max'' (x:xs) = max x $ max'' xs

-- Exercise 7.13

-- Test data for "ins" function



-- Exercise 7.14

isSorted [] = True
isSorted [x] = True
isSorted (x:xs) = x <= (head xs) && (isSorted xs)

-- Exercise 7.15

{- Properties for a sort function
   1. The result is sorted.
   2. The result is the same length as the input.
   3. The result contains all the elements that are in the input.
   4. The result contains the same number of each element in the input.
-}

prop_isSorted xs = isSorted $ iSort xs
prop_sameLength xs = length xs == length (iSort xs)
prop_sameElements xs = and [elem x xs | x <- (iSort xs)]
prop_sameNumElements xs = and [ elemNum x xs == elemNum x ys  | x <- xs]
                     where ys = iSort xs

-- Exercise 7.16




-- Exercise 7.17


-- Exercise 7.18




-- Exercise 7.19

