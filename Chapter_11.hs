module Chapter_11 
where

import Chapter_06

import Data.List
-- Exercise 11.1

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g

-- Using standard composition
produceBill :: TillType -> String
produceBill = formatBill . makeBill

-- Using forward composition >.>
produceBill' :: TillType -> String
produceBill' = makeBill >.>  formatBill


-- Exercise 11.2

{-
    
    All these functions are the same - they all resolve to f.

-}

-- Exercise 11.3

{-
If we have functions:
    f :: a -> b
    g :: b -> c
    h :: c -> d

then the general composition h.g.f is

    h.g.f :: a -> d

Example:
-}

myToInt :: Char -> Int
myToInt c = case c of 
                'a' -> 100
                'b' -> 200
                _   -> 300

vat :: Int -> Double
vat x = 1.23 * (fromInteger (toInteger x))

printDouble :: Double -> [Char]
printDouble d
    | d > 200.0 = "More than (>) two hundred"
    | otherwise = "Less than (<) two hundred"

{-
    composed_function = printDouble . vat . myToInt :: Char -> [Char]

In order for use to have a list of functions, the list must be homogeneous, so at best it
must be of the form:

    [(a -> b)]

However, the output of one function feeds into the next, so both a and b must be of
the same type:

    [(a -> a)]

-}

-- Using recursion
composeList :: [(a -> a)] -> (a -> a)
composeList [] = id
composeList (f:fs) = f . composeList fs

-- Using a fold
composeList' :: [(a -> a)] -> (a -> a)
composeList' fs = foldr (.) id fs

-- Test functions
addOne :: Int -> Int
addOne a = a + 1

mulTen :: Int -> Int
mulTen a = a * 10

subThree :: Int -> Int
subThree a = a - 3

funcList = [subThree, mulTen, addOne]

-- Exercise 11.4

{-
    :t ($)
    ($) :: (a -> b) -> a -> b

-}

-- Exercise 11.5

result = zipWith ($) [sum, product] [[1, 2], [3, 4]]

{-
    This gives the sum of the integers in the first element, and the product of the
    integers in the second element:

    [($) sum [1, 2], ($) product [3, 4]]
    [3, 12]

-}

-- Exercise 11.6

{-

   (id $ f)     -- This is the same as f

   (f $ id)     -- Doesn't make sense? But you can say (if f :: Int -> b)
                -- f $ id 7

   id ($)       -- This is the same as $

-}

-- Exercise 11.7


-- Exercise 11.8


