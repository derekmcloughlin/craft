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
             deriving(Eq, Ord, Show)

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

