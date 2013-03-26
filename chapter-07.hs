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

unique' :: [Integer] -> [Integer]

-- TODO: this doesn't work.
unique' [] = []
unique' [x] = [x]
unique' (x:xs)
    | elemNum' x xs == 0  = x : unique' xs
    | otherwise = unique' xs


-- List Comprehensions

unique'' :: [Integer] -> [Integer]

unique'' xs = [x | x <- xs, elemNum' x xs == 1]


-- Exercise 7.10

prop_unique'' xs = and [elemNum' x xs == 1 | x <- unique'' xs]


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

testIns1 = (5, [1, 2, 3, 4]) -- at the end
testIns2 = (3, [1, 2, 4, 5]) -- in the middle
testIns3 = (1, [2, 3, 4, 5]) -- at the start
testIns4 = (1, [])           -- into empty list
testIns5 = (1, [2])          -- single element list before
testIns6 = (2, [1])          -- single element list after

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

-- ins that sorts in descending order
iSortDesc :: [Integer] -> [Integer]

iSortDesc [] = []
iSortDesc (x: xs) = insDesc x (iSortDesc xs)

insDesc :: Integer -> [Integer] -> [Integer]
insDesc x [] = [x]
insDesc x (y:ys)
    | x >= y    = x:(y:ys)
    | otherwise = y : insDesc x ys


iSortUnique :: [Integer] -> [Integer]

iSortUnique [] = []
iSortUnique (x: xs) = insUnique x (iSortUnique xs)

insUnique :: Integer -> [Integer] -> [Integer]
insUnique x [] = [x]
insUnique x (y:ys)
    | x < y     = x:(y:ys)
    | x == y    = ys
    | otherwise = y : insUnique x ys


-- Exercise 7.17

{- No reall need to modify original definition of iSort unless you wanted to
   keep the other definitions.
-}

-- Exercise 7.18

-- Test data for iSortUnique

testiSortUnique_1 = []
testiSortUnique_2 = [1]
testiSortUnique_3 = [1, 1]
testiSortUnique_4 = [1, 2, 3, 4]    -- already unique (and sorted)
testiSortUnique_5 = [1, 3, 4, 2]    -- already unique but not sorted
testiSortUnique_6 = [1, 1, 4, 3, 2] -- first element duplicated
testiSortUnique_7 = [1, 3, 4, 2, 2] -- last element duplicated


-- Exercise 7.19

-- Sorting lists of pairs of numbers

iSortPairs :: [(Integer, Integer)] -> [(Integer, Integer)]
iSortPairs [] = []
iSortPairs ((x, y) : xs) = insPair (x, y) (iSortPairs xs)


insPair :: (Integer, Integer) -> [(Integer, Integer)] -> [(Integer, Integer)]
insPair (x, y) [] = [(x, y)]
insPair (x, y) ((x1, y1):ys)
    | x < x1 || ( x == x1 && y <= y1)  = (x, y) : ((x1, y1) : ys)
    | otherwise                        = (x1, y1) : insPair (x, y) ys


-- Exercise 7.20

-- Redefining "drop" and "splitAt" in the same way as "take"

take' :: Int -> [a] -> [a]

take' 0 _      = []
take' _ []     = []
take' n (x:xs)
    | n > 0    = x : take (n - 1) xs
take' _ _ = error "take': negative argument"

drop' :: Int -> [a] -> [a]

drop' 0 xs     = xs
drop' _ []     = []
drop' n (x:xs)
    | n > 0    = drop' (n - 1) xs 
drop' _ _ = error "drop': negative argument"


splitAt' :: Int -> [a] -> ([a], [a])

splitAt' n xs = (take' n xs, drop' n xs)

-- Exercise 7.21

{-
    ghci> take' (-3) []
    []
    
Redefine:

take' n []
    | n > 0     = []
    | n == 0    = []
    | otherwise = error "take': negative argument"
-}


-- Exercise 7.22

zip' :: ([a], [b]) -> [(a, b)]

zip' (xs, ys) = zip xs ys

{- Sometimes you lose information when using zip'
   E.g.
        zip' ([1, 2, 3, 4], ['e', 'f', 'g']
        ([1,2,3],"efg")

   We lose the '4', and unzipping won't get it back.

   The best we can hope for is that either the first item in the pair
   or the 2nd are recovered.
 -}

prop_zipunzip (xs, ys) = fst (unzip (zip' (xs, ys))) == xs
                         ||
                         snd (unzip (zip' (xs, ys))) == ys
                         
-- Exercise 7.23

-- TODO
{-
zip3' :: ([a], [b], [c]) -> [(a, b, c)]

zip3' ([] [] []) = []
zip3' (x:xs) (y:ys) (z:zs) = (x, y, z) : zip3' xs ys zs
zip3'  _ = []
-}


-- Exercise 7.24

qSort :: [Integer] -> [Integer]

qSort [] = []
qSort (x:xs) = qSort [y | y <- xs, y <= x] ++ [x] ++ qSort [y | y <- xs, y > x]

qSortDesc :: [Integer] -> [Integer]

qSortDesc [] = []
qSortDesc (x:xs) = qSortDesc [y | y <- xs, y > x] ++ [x] ++ qSortDesc [y | y <- xs, y <= x]


qSortNoDups :: [Integer] -> [Integer]

qSortNoDups [] = []
qSortNoDups (x:xs) = unique'' $ qSortNoDups [y | y <- xs, y <= x] ++ [x] ++ qSortNoDups [y | y <- xs, y > x]


-- Exercise 7.25

isSubList :: Eq a => [a] -> [a] -> Bool

isSubList [] _ = True
isSubList _ [] = False
isSubList (x:xs) (y:ys)
    | x == y     = isSubList xs ys
    | otherwise  = isSubList (x:xs) ys


isSubSequence :: Eq a => [a] -> [a] -> Bool

isSubSequence [] _ = True
isSubSequence _ [] = False
-- Note: can't really use a guard here because we want both parts of the 'or' condition to
-- be evaluated sometimes.
isSubSequence (x:xs) (y:ys) = (x == y && isSubSequence xs ys) || (isSubSequence (x:xs) ys)


-- Exercise 7.26

prop_sublist1 [] _ = True      -- Quickcheck might pass in an empty list for the 1st param
prop_sublist1 xs ys = isSubList xs xs == True -- But here we don't care about the 2nd param

prop_subsequence1 [] _ = True  -- Quickcheck might pass in an empty list for the 1st param
prop_subsequence1 xs ys = isSubSequence xs (ys ++ xs) == True -- But here we don't care about the 2nd param

-- Exercise 7.27



-- Exercise 7.28



-- Exercise 7.29



-- Exercise 7.30



-- Exercise 7.31



-- Exercise 7.32



-- Exercise 7.33



-- Exercise 7.34



-- Exercise 7.35



-- Exercise 7.36



