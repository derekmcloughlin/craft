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

sum_of_squares':: Integer -> Integer
sum_of_squares' n = foldr (+) 0 $ map (^2) [1..n]

-- Exercise 10.14

sum_sq_pos_int :: [Integer] -> Integer
sum_sq_pos_int xs = foldr (+) 0 [x*x | x <- xs, x > 0]

-- Exercise 10.15

unzip' :: [(a, b)] -> ([a], [b])
unzip' xs = (foldr first [] xs, foldr second [] xs)
    where first z zs = [fst z] ++ zs
          second z zs = [snd z] ++ zs

-- There should be a better way to do this.
-- What we're effectively doing is reversing the list then
-- taking the head.
last' :: Eq a => [a] -> a
last' xs = head $ foldr p [] xs

p :: Eq a => a -> [a] -> [a]
p x y
    | y == [] = [x]
    | otherwise = y

{-
init' :: [a] -> [a]
init' xs = tail $ foldr q [] xs

q :: a -> [a] -> [a]
q x y 
    | y == [] = [x]
    | otherwise = y
-}

-- Exercise 10.16

mystery xs = foldr (++) [] (map sing xs)
    where sing x = [x]

-- It just takes an array and returns an indentical array


-- Exercise 10.17

-- From Chapter 6:

type Name = String
type Price = Int

type LineItem = (Name, Price)

formatLine :: LineItem -> String
formatLine (name, price) = name ++ replicate n '.' ++ priceStr ++ "\n"
                           where n = 30 - (length name) - (length priceStr)
                                 priceStr = formatPence price

formatPence :: Price -> String
formatPence n = (show pounds) ++ "." ++ (pad0 pence ) 
            where pounds = n `div` 100
                  pence = n `mod` 100
                  pad0 p = if p < 10 then
                              "0" ++ show (p)
                           else
                              show p

formatLines :: [LineItem] -> String
formatLines lines = concat [formatLine line | line <- lines]

-- test data
shopping :: [LineItem]
shopping = [("Dry Sherry, 1Lt", 540),
            ("Wet Sherry ,2Lt", 1234),
            ("Pink Lemonade, 3Lt", 403)]


formatList :: (a -> String) -> [a] -> String
formatList f xs = foldr (++) [] (map f xs)

formatLines' :: [LineItem] -> String
formatLines' lines = formatList formatLine lines


-- Exercise 10.18

filterFirst :: (a -> Bool) -> [a] -> [a]
filterFirst p (x:xs)
    | p x == False  = xs
    | otherwise     = x : filterFirst p xs

{-
    ghci> filterFirst isDigit "123"
    "123*** Exception: chapter-10.hs:(256,1)-(258,42): Non-exhaustive patterns in function filterFirst
    Add the line below to get it working.
-}
filterFirst p [] = []

-- Exercise 10.19

filterLast :: (a -> Bool) -> [a] -> [a]
filterLast p [] = []
filterLast p xs
    | p (last xs) == False  = init xs
    | otherwise             = (filterLast p (init xs)) ++ [last xs]

-- In terms of filterFirst
filterLast' :: (a -> Bool) -> [a] -> [a]
filterLast' p xs = reverse $ filterFirst p $ reverse xs

-- Exercise 10.20

switchMap :: (a -> b) -> (a -> b) -> [a] -> [b]
switchMap _ _ [] = []
switchMap f g (x:xs) = f x : switchMap g f xs

-- TODO: Re-implement using foldr?

-- Exercise 10.21

split :: [a] -> ([a], [a])
split xs =  (foldr p [] xs, foldr q [] xs)
    where p z zs = [z] ++ [head zs]
          q y ys = [y]  -- ++ [head (tail ys)]


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



