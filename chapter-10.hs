import Data.List

-- Exercise 10.1

doubleAllLC :: [Integer] -> [Integer]
doubleAllLC xs = [2 * x | x <- xs] 

doubleAllRecur :: [Integer] -> [Integer]
doubleAllRecur [] = []
doubleAllRecur (x:xs) = 2 * x : doubleAllRecur xs

doubleAllMap :: [Integer] -> [Integer]
doubleAllMap xs = map (2*) xs

{-

doubleAllLC [2, 1, 7]
~ [2 * 2, 2 * 1, 2 * 7]
~ [4, 2, 14]

doubleAllRecur [2, 1, 7]
~ 2 * 2 : doubleAllRecur [1, 7]
~ 2 * 2 : 2 * 1 : doubleAllRecur [7]
~ 2 * 2 : 2 * 1 : 2 * 7 : doubleAllRecur []
~ 2 * 2 : 2 * 1 : 2 * 7 : []
~ [2 * 2, 2 * 1, 2 * 7]
~ [4, 2, 14]

doubleAllMap [2, 1, 7]
~ [ (2*) 1, (2*) 1, (2*) 7]
~ [4, 2, 14]

-}


-- Exercise 10.2

length' :: [a] -> Integer
length' xs = sum (map one xs)
             where
             one _ = 1

-- Exercise 10.3

addUp ns = filter greaterOne (map addOne ns)
           where
           greaterOne n = n > 1
           addOne n     = n + 1

addUp' ns = map addOne (filter greaterZero ns)           
            where
            greaterZero n = n > 0
            addOne n      = n + 1

-- Exercise 10.4

someFn1 ns = map addOne (map addOne ns)
            where
            addOne n      = n + 1

-- Adds 2 to each number in ns

-- Exercise 10.5

someFn2 ns = filter greaterOne (filter lessTen ns)
             where
             greaterOne n = n > 1
             lessTen n    = n < 10

{-
    Only numbers between 1 and 10 but not inclusive, are filtered.

    filter p (filter q xs) => filter (p and q) ns
-}
          

-- Exercise 10.6

squareThem :: [Integer] -> [Integer]
squareThem xs = map (^2) xs

sumOfSquares :: [Integer] -> Integer
sumOfSquares xs = sum (squareThem xs)

greaterThanZero :: [Integer] -> Bool
greaterThanZero xs = length xs == length (filter ( > 0 ) xs)

-- Exercise 10.7

type Func = Integer -> Integer

test_min_f :: Func -> Integer -> Integer
test_min_f f n = minimum (map f [0..n])

test_eq_f :: Func -> Integer -> Bool
test_eq_f f n = length (filter (== (f 0)) (map f [0..n])) == length [0..n]

test_gt0_f :: Func -> Integer -> Bool
test_gt0_f f n = length (filter (> 0) (map f [0..n])) == length [0..n]

test_increasing_f :: Func -> Integer -> Bool
test_increasing_f f n = (map f [0..n]) == sort (map f [0..n])

some_f :: Func
some_f x = 100 - x + 1

const_f :: Func
const_f _ = 3

inc_f :: Func
inc_f x = x + 1 -- always increasing

dec_f :: Func
dec_f x = 100 - x -- always increasing for x < 100


-- Exercise 10.8

twice :: (Integer -> Integer) -> Integer -> Integer
twice f n = f ( f n)

twiceGeneral :: (a -> a) -> a -> a
twiceGeneral f x = f (f x)


-- Exercise 10.9

iter :: Integer -> (a -> a) -> a -> a

iter 0 f x = x
iter n f x = f $ iter (n - 1) f x

-- Exercise 10.10

double :: Integer -> Integer
double x = 2 * x

twoN :: Integer -> Integer
twoN n = iter n double 1

-- Exercise 10.11

{-
Quickcheck for

filter p xs

prop_filter_1 p xs = length [1 | x <- xs, p x == False] == 0

    
-}

-- Exercise 10.12


{-
Quickcheck for
    f (g x) ~ x
    g (f y) ~ y

    map f xs

prop_fg_1 f g xs = map g (map f xs) == map f (map g xs)

-}

-- Exercise 10.13

sum_of_squares_map :: Integer -> Integer
sum_of_squares_map n = sum $ map (^2) [1..n]

sum_of_squares_foldr :: Integer -> Integer
sum_of_squares_foldr n = sum $ foldr (^2) [1..n]

-- Exercise 10.14



-- Exercise 10.15



-- Exercise 10.16



-- Exercise 10.17



-- Exercise 10.18



-- Exercise 10.19



-- Exercise 10.20



-- Exercise 10.21



-- Exercise 10.22



-- Exercise 10.23



-- Exercise 10.24



-- Exercise 10.25



-- Exercise 10.26



-- Exercise 10.27



-- Exercise 10.28



-- Exercise 10.29



-- Exercise 10.30



-- Exercise 10.31



-- Exercise 10.32



-- Exercise 10.33



-- Exercise 10.34



-- Exercise 10.35



-- Exercise 10.36



-- Exercise 10.37



