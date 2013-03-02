import Pictures
import Test.QuickCheck
import Test.HUnit


-- Exercise 5.1

maxOccurs :: Integer -> Integer -> (Integer, Integer)

maxOccurs x y
    | x == y      = (x, 2)
    | otherwise   = (max x y, 1)

maxThreeOccurs :: Integer -> Integer -> Integer -> (Integer, Integer)

maxThreeOccurs x y z
    | x == y && y == z    = (x, 3)
--    | x == y || x == z || y == z    = 
    | otherwise           = (max (max x y) z, 1)

-- Exercise 5.2

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max (max x y ) z

minThree :: Integer -> Integer -> Integer -> Integer
minThree x y z = min (min x y) z

middle :: Integer -> Integer -> Integer -> Integer
middle x y z
    | (y <= x && x <= z)  || (z <= x && x <= y)       = x
    | (x <= y && y <= z)  || (z <= y && y <= x)       = y
    | (y <= z && z <= x)  || (x <= z && z <= y)       = z

orderTriple :: (Integer, Integer, Integer) -> (Integer, Integer, Integer)

orderTriple (x, y, z) = (minThree x y z, middle x y z, maxThree x y z)

-- Exercise 5.3

-- Straight line formula: y = ax + b
-- Root: ax + b = 0
--       x = -b/a
-- No root for a == 0

straightLineRoot :: Integer -> Integer -> (Float, Bool)

straightLineRoot a b
    | a == 0     = (0, False)
    | otherwise  = (-(fromInteger b)/(fromInteger a), True)

-- Exercise 5.4

testOrderTriple1 = TestCase (assertEqual "for: orderTriple (1, 2, 3)" (1, 2, 3) (orderTriple (1, 2,  3)))
testOrderTriple2 = TestCase (assertEqual "for: orderTriple (3, 2, 1)" (1, 2, 3) (orderTriple (3, 2,  1)))
testOrderTriple3 = TestCase (assertEqual "for: orderTriple (2, 1, 3)" (1, 2, 3) (orderTriple (2, 1,  3)))

testsOrderTriple = TestList [testOrderTriple1, testOrderTriple2, testOrderTriple3]


-- Unfortunately you can't pattern match with _ in the following case
-- I tried 
-- testRoot1 = TestCase (assertEqual "for: straightLineRoot 0 3" (_, False) (straightLineRoot 0 3))
-- without success

testRoot1 = TestCase (assertEqual "for: straightLineRoot 0 3" False (snd (straightLineRoot 0 3)))
testRoot2 = TestCase (assertEqual "for: straightLineRoot 1 3" True  (snd (straightLineRoot 1 3)))
testRoot3 = TestCase (assertEqual "for: straightLineRoot 1 0" True  (snd (straightLineRoot 1 0)))

testsRoot = TestList [testRoot1, testRoot2, testRoot3]


-- Exercise 5.5

-- Length of perimeter of a geometrical shape

data Shape = Circle Float |
             Rectangle Float Float |
             Triangle Float Float Float
             deriving(Ord, Show)

perimeter :: Shape -> Float
perimeter (Circle r )           = 2 * pi * r
perimeter (Rectangle x y)       = 2 * (x + y)
perimeter (Triangle a b c)      = (a + b + c)

{- In GHCI:

   ghci> perimeter (Circle 10)
   62.831856
   ghci> perimeter (Rectangle 10 10)
   40.0
   ghci> perimeter $ Rectangle 10 10
   40.0
-}



-- Exercise 5.6

data ShopItem = ShopItem String Float
                deriving (Eq, Show)
                
name :: ShopItem -> String

name (ShopItem item_name _) = item_name

price :: ShopItem -> Float

price (ShopItem _ item_price) = item_price

{-
    ghci> let a = ShopItem "Bananas" 4.35
    ghci> a
    ShopItem "Bananas" 4.35
    ghci> name a
    "Bananas"
    ghci> price a
    4.35
-}

-- Exercise 5.7

-- Added "Triangle" to constructor of shape in Ex 5.5 because can't redefine Shape here.

isRound :: Shape -> Bool
isRound (Circle _)        = True
isRound (Rectangle _ _)   = False
isRound (Triangle _ _ _)  = False

-- Note: Couldn't do the following:
-- isRound (_ _)       = False

area :: Shape -> Float

area (Circle r)         = pi * r * r
area (Rectangle x y)    = x * y
area (Triangle a b c)   = sqrt (s * (s - a) * (s - b) * (s - c))
                          where s = (a + b + c ) / 2

-- Exercise 5.8

regular :: Shape -> Bool

regular (Circle _)        = True

regular (Rectangle x y)
    | x == y              = True
    | otherwise           = False

regular (Triangle a b c)
    | a == b && b == c    = True
    | otherwise           = False


-- Exercise 5.9

-- Derived definitions (

data Move = Rock |
            Paper |
            Scissors
            deriving (Show, Eq)

{-
    In GHCI:

    ghci> let a = Rock
    ghci> let b = Paper
    ghci> let c = Scissors
    ghci> a
    Rock                -- This is the 'Show' function
    ghci> a == b        -- This is the 'Eq' function
    False
    ghci> let d = Rock
    ghci> a == d
    True
    
-}

{-
    For the Shape type (Show, Eq, Ord)

    ghci> let a = Circle 3
    ghci> let b = Circle 2
    ghci> a                     -- 'Show'
    Circle 3.0
    ghci> a == b                -- 'Eq'
    False
    ghci> let c = Circle 2
    ghci> b == c
    True
    ghci> a < b                 -- 'Ord'
    False
    ghci> a > b                 
    True
    ghci> b > c
    False
    ghci> b < c
    False

    The following compares Circles and Rectangles. Doesn't really make much sense here.
    Ordering goes by the order of the constructors. This makes sense in enumerated
    types such at Month = January | February | March | ...
    
    ghci> let d = Rectangle 2 3
    ghci> a < d
    True
    ghci> a == d
    False
    ghci> d < a
    False
    
-}

-- Exercise 5.10

iseq_ual :: Shape -> Shape -> Bool
iseq_ual (Circle a) (Circle b)
     | a == b           = True
     | -a == b          = True
     | otherwise        = False

-- could also use the 'abs' function
iseq_ual (Rectangle a b) (Rectangle c d)
     | abs a == abs c && abs b == abs d  = True
     | otherwise                         = False
     
-- Correct way to do this using == operator
-- Need to remove the deriving Eq in the definition of Shape above
instance Eq Shape where 
    (Circle x) == (Circle y)            =  abs x == abs y
    (Rectangle a b) == (Rectangle c d)  =  abs a == abs b && abs c == abs d


-- Exercise 5.11


data NewShape = NewCircle Float (Float, Float) |
                NewRectangle Float Float (Float, Float) |
                NewTriangle Float Float Float (Float, Float)
                deriving(Eq, Ord, Show)


-- Exercise 5.12

move :: Float -> Float -> NewShape -> NewShape

move x1 y1  (NewCircle r (x, y))        = NewCircle r (x + x1, y + y1)
move x1 y1  (NewRectangle w h (x, y))   = NewRectangle w h (x + x1, y + y1)
move x1 y1  (NewTriangle a b c (x, y))  = NewTriangle a b c (x + x1, y + y1)

-- Exercise 5.13

overlapping :: NewShape -> NewShape -> Bool

overlapping (NewCircle r1 (x1, y1)) (NewCircle r2 (x2, y2)) = True
            

-- Exercise 5.14

data House = HouseName String |
             HouseNumber Integer
             deriving (Show)


toText :: House -> String
toText (HouseName a)    = a
toText (HouseNumber a)  = show a

data Address = Address House String String
               deriving (Show)

my_house_no = HouseNumber 123

my_house = Address my_house_no "O'Connell Street" "Dublin 2"

-- Exercise 5.15

{-
    ghci> [0, 0.1 .. 1]
    [0.0,0.1,0.2,0.30000000000000004,0.4000000000000001,0.5000000000000001,0.6000000000000001,0.7000000000000001,0.8,0.9,1.0]

-}

-- Exercise 5.16

{-
    ghci> length [2, 3]
    2
    ghci> length [[2, 3]]
    1
    ghci> :t [[2, 3]]
    [[2, 3]] :: Num t => [[t]]
-}

-- Exercise 5.17

{-
    ghci> [2 .. 2]
    [2]
    ghci> [2, 7 .. 4]
    [2]
    ghci> [2, 2 .. 2]
    [2,2,2,2,2,2,2,2,2,2,............. goes on forever
-}

-- Exercise 5.18

doubleAll :: [Integer] -> [Integer]
doubleAll xs = [x * 2 | x <- xs]

-- Exercise 5.19

-- Take these definitions from Exercise 3.16
offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

toUpper :: Char -> Char
toUpper ch = toEnum(fromEnum ch + offset)

isLower :: Char -> Bool
isLower ch = 'a' <= ch && ch <= 'z' 

isAlpha :: Char -> Bool
isAlpha ch = ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') 

smallToCapitals :: Char -> Char

smallToCapitals ch
        | isLower ch = toUpper ch
        | otherwise = ch

capitalize :: String -> String

capitalize xs = [smallToCapitals x | x <- xs]

capitalizeLetters xs = [smallToCapitals x | x <- xs, isAlpha x]

-- Exercise 5.20

divisors :: Integer -> [Integer]

divisors n = [x | x <- [1 .. n], n `rem` x == 0]

isPrime :: Integer -> Bool

isPrime n = [1, n] == divisors n

-- Exercise 5.21

matches :: Integer -> [Integer] -> [Integer]

matches n xs = [x | x <- xs, x == n]

-- "elem" conflicts with Prelude function. I prefer just to give it a different name
-- rather than hiding it.
elem' :: Integer -> [Integer] -> Bool

elem' n xs = matches n xs /= []

-- Exercise 5.22

onSeparateLines :: [String] -> String

onSeparateLines xs = [ch | x <- xs, ch <- x ++ "\n"]

{-
    ghci> putStr $ onSeparateLines ["Hello", "World"]
    Hello
    World
-}

-- Exercise 5.23

duplicate :: String -> Integer -> String

duplicate xs n
    | n == 0        = ""
    | n == 1        = xs
    | otherwise     = [x | x <- xs, c <- [1..n] ]

{-  Note: the order of the conditions in the list comprehension is important:

    ghci> duplicate "hello" 3
    "hellohellohello"

    Our order goes through the [1..n] first:

    | otherwise     = [x | c <- [1..n], x <- xs ]

    However, if we had the order in the 'otherwise' part as follows:

    | otherwise     = [x | x <- xs, c <- [1..n] ]

    You'd get:
    
    ghci> duplicate "hello" 3
    "hhheeellllllooo"
-}

-- Exercise 5.24

-- Exercise 5.25

-- Exercise 5.26

-- Exercise 5.27

-- Exercise 5.28

-- Exercise 5.29

-- Exercise 5.30

-- Exercise 5.31

-- Exercise 5.32

-- Exercise 5.33

-- Exercise 5.34

