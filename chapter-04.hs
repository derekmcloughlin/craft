-- Exercise 4.1
import Pictures
import Test.QuickCheck

maxThree :: Integer -> Integer -> Integer -> Integer
maxThree x y z = max (max x y ) z

maxFour_1, maxFour_2, maxFour_3  :: Integer -> Integer -> Integer -> Integer -> Integer

maxFour_1 x y z w = maxThree x y (maxThree y z w)

maxFour_2 x y z w = max (max (max x y) z) w

maxFour_3 x y z w = max (maxThree x y z) w


-- Exercise 4.2

between :: Integer -> Integer -> Integer -> Bool
between x y z
        | weakAscendingOrder x y z = True
        | otherwise                = False

weakAscendingOrder :: Integer -> Integer -> Integer -> Bool
weakAscendingOrder m n p
        | (m <= n && n <= p)    = True
        | otherwise             = False


-- Exercise 4.3

howManyEqual :: Integer -> Integer -> Integer -> Integer
howManyEqual x y z
             | x == y && y == z   = 3
             -- Now test the condition that might have failed
             | x == y || y == z   = 2
             | otherwise          = 0


-- Exercise 4.4

howManyOfFourEqual :: Integer -> Integer -> Integer -> Integer -> Integer

howManyOfFourEqual a b c d
                   -- Note: this logic depends on the fact that
                   -- guard conditions are done in the order that they
                   -- appear in the file.

                   -- For 4 all equal
                   | a == b && b == c && c == d         = 4

                   -- Combinations of 3 equal
                   | a == b && b == c && c /= d         = 3
                   | a == b && b == d && c /= d         = 3
                   | a == c && c == d && b /= d         = 3
                   | b == c && c == d && a /= d         = 3

                   -- Combinations of 2 equal
                   | a == b && b /= c && c /= d         = 2
                   | a == c && c /= b && c /= d         = 2
                   | a == d && b /= c && c /= d         = 2
                   | b == c && b /= d && a /= d         = 2
                   | b == d && b /= a && b /= c         = 2
                   | c == d && b /= c && a /= c         = 2

                   | otherwise                          = 0

-- Exercise 4.5

-- Original function
fourPics :: Picture -> Picture

fourPics pic =
    left `beside` right
    where
        stack p = p `above` invertColour p
        left	= stack pic
        right	= stack (invertColour (flipV pic))

-- Other implementations
fourPics' pic =
    left `beside` right
    where
    left = pic `above` (invertColour pic)
    right = (invertColour (flipV left))

fourPics'' pic =
    left `beside` right
    where
    left = pic `above` (invertColour pic)
    right = (flipV (invertColour left))

-- Exercise 4.6

fourPics_alt :: Picture -> Picture

fourPics_alt pic =
    top `above` bottom
    where
    stack p     = p `beside` invertColour (flipV p)
    top         = stack pic
    bottom	= stack (invertColour pic)


-- Exercise 4.7
-- Two other ways to do the fourPics function

-- Exercise 4.8

possible :: Int -> Int -> Int -> Bool

possible a b c
    | (a + b > c) &&  (a + c > b) && (b + c > a)    = True
    | otherwise                                     = False


-- Exercise 4.9

maxThreeOccurs :: Int -> Int -> Int -> (Int, Int)

maxThreeOccurs x y z =
    (theMax, numMaxes)
    where
    theMax = max x (max y z)
    numMaxes = (if x == theMax then 1 else 0)
             + (if y == theMax then 1 else 0)
             + (if z == theMax then 1 else 0)
               
-- Exercise 4.10

{-
maxThreeOccurs 4 5 5
maxThreeOccurs 4 5 4
-}

-- Exercise 4.11

data Move = Rock |
            Paper |
            Scissors
            deriving (Show, Eq)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat _ = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

data RPS_Result = Win |
                  Lose |
                  Draw
                  deriving (Show, Eq)

opposite :: RPS_Result -> RPS_Result
opposite Win = Lose
opposite Lose = Win
opposite Draw = Draw

-- Exercise 4.12

outcome :: Move -> Move -> RPS_Result

outcome Rock Scissors     = Win
outcome Paper Rock        = Win
outcome Paper Scissors    = Win
outcome Scissors Rock     = Lose
outcome Rock Paper        = Lose
outcome Scissors Paper    = Lose
outcome Rock Rock         = Draw
outcome Paper Paper       = Draw
outcome Scissors Scissors = Draw

-- Exercise 4.13

-- First, the 'magic' needed for QuickCheck to do its work

instance Arbitrary Move where
    arbitrary = elements [Rock, Paper, Scissors]

prop_beat m1 = (beat (beat (beat m1)) == m1)

prop_beat_lose m1	=  (lose m1 == beat (beat m1))
                        && (beat m1 == lose (lose m1))

-- Exercise 4.14

prop_outcome m1 m2      =  (outcome m1 m2 == opposite (outcome m2 m1))
                        && (outcome m1 m1 == Draw)

-- Exercise 4.15

data Season = Spring |
              Summer |
              Autumn |
              Winter
              deriving (Eq, Show, Ord)

data Temp = Cold |
            Hot
            deriving (Eq, Show, Ord)

seasonalTemp :: Season -> Temp

seasonalTemp Spring = Cold
seasonalTemp Summer = Hot
seasonalTemp Autumn = Cold
seasonalTemp Winter = Cold

-- Exercise 4.16

data Month = January    |
             February   |
             March      |
             April      |
             May        |
             June       |
             July       |
             August     |
             September  |
             October    |
             November   |
             December   
             deriving (Eq, Show, Ord)


monthToSeason :: Month -> Season
monthToSeason m
              | m == December || m == January || m == February  = Winter
              | m == March    || m == April   || m == May       = Spring
              | m == June     || m == July    || m == August    = Summer
              | otherwise = Autumn

-- Exercise 4.17

rangeProduct :: Integer -> Integer -> Integer

rangeProduct m n
             | m < n   = m * rangeProduct (m + 1) n
             | n == m  = n
             | n < m   = 0

-- Exercise 4.18

-- original definition of fac
fac :: Integer -> Integer
fac n
    | n == 0    = 1
    | n > 0     = fac (n - 1) * n
    | otherwise = 0

fac' :: Integer -> Integer
fac' n = rangeProduct 1 n

-- Exercise 4.19

recurMul :: Integer -> Integer -> Integer

recurMul x y
         | x == 1       = y
         | otherwise    = y + recurMul (x - 1) y

-- Exercise 4.20

intSqrRoot :: Integer -> Integer

intSqrRoot n = 3
