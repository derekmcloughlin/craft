-- Exercise 4.1
import Pictures
import Test.QuickCheck
import Test.HUnit

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

-- primitive recursive definition
intSqrRoot n = intSqrRootHelper n n
           where
           intSqrRootHelper n m 
                | m == 1                                         = 1
                | m > 1 && (m - 1) * (m - 1) <= n && m * m < n   = m
                | otherwise                                      = intSqrRootHelper n (m - 1)

-- Exercise 4.21

f :: Integer -> Integer

f 0  = 0
f 1  = 44
f 2  = 17
f 3  = 17
f 4  = 55
f 6  = 17
f 7  = 19
f _  = 0

maxf :: (Integer -> Integer) -> Integer -> Integer
maxf f n
     | n > 0    = max (f n) (maxf f (n - 1))
     | n == 0   = f 0

-- Exercise 4.22

f2 :: Integer -> Integer

f2 0 = 1
f2 1 = 1
f2 2 = 2
f2 3 = 0
f2 4 = 4
f2 5 = 3
f2 _ = 5

isZeroF :: (Integer -> Integer) -> Integer -> Bool

isZeroF f n
        | n > 0         = (f n) == 0 || isZeroF f (n - 1)
        | n == 0        = (f 0) == 0


-- Exercise 4.23

-- Original regions code

regions :: Integer -> Integer
regions n
        | n == 0        = 1
        | n > 0         = regions (n - 1) + n

-- Original sumFun code

sumFun :: (Integer -> Integer) -> Integer -> Integer
sumFun f n
       | n == 0         = f 0
       | n > 0          = sumFun f (n - 1) + f n


regions' :: Integer -> Integer

regions' n = sumFun (+ 0) n + 1

-- Test that they're the same
-- [(a, regions a, regions' a) | a <- [1..10]]

-- Exercise 4.24

-- Max no of pices for a solid block

----------------
-- For the next exercises 
empty = ["",
         "",
         "",
         "",
         "",
         ""]

-- Exercise 4.25

blackSquares :: Integer -> Picture

blackSquares n = nSquares black n

whiteSquares :: Integer -> Picture

whiteSquares n = nSquares white n

nSquares :: Picture -> Integer -> Picture
nSquares p n
    | n == 0    = empty
    | n <= 1    = p
    | otherwise = p `beside` nSquares p (n - 1)

blackWhite :: Integer -> Picture
blackWhite n
    | n <= 1    = black
    | otherwise = black `beside` whiteBlack (n - 1)

whiteBlack :: Integer -> Picture

whiteBlack n
    | n <= 1    = white
    | otherwise = white `beside` blackWhite (n - 1)

blackChess :: Integer -> Integer -> Picture

blackChess n m
    | n <=1     = blackWhite m
    | otherwise = blackWhite m `above` whiteChess (n - 1) m

whiteChess :: Integer -> Integer -> Picture
whiteChess n m
    | n <=1     = whiteBlack m
    | otherwise = whiteBlack m `above` blackChess (n - 1) m

-- Exercise 4.26

column :: Picture -> Integer -> Picture

column p n
    | n <= 1    = p
    | otherwise = p `above` column p (n - 1)

-- Exercise 4.27

diagonal_LR :: Integer -> Picture

diagonal_LR n = diagonalHelper n n
    where
    diagonalHelper n m
        | m == 0   = empty
        | m >= 1   = ((whiteSquares (n - m)) `beside`
                           black `beside`
                           whiteSquares (m - 1))
                           `above` diagonalHelper n (m - 1)

-- Exercise 4.28

diagonal_RL :: Integer -> Picture

diagonal_RL n = diagonalHelper n n
    where
    diagonalHelper n m
        | m == 0   = empty
        | m >= 1   = ((whiteSquares (m - 1)) `beside`
                           black `beside`
                           whiteSquares (n - m))
                           `above` diagonalHelper n (m - 1)


-- Exercise 4.29

crossDiagonal :: Integer -> Picture

crossDiagonal n = superimpose (diagonal_LR n) (diagonal_RL n)

        
-- Exercise 4.30

whiteBlackCol :: Integer -> Picture
whiteBlackCol n
    | n <= 1    = white
    | otherwise = white `above` blackWhiteCol (n - 1)

blackWhiteCol :: Integer -> Picture
blackWhiteCol n
    | n <= 1    = black
    | otherwise = black `above` whiteBlackCol (n - 1)

chessBoard :: Integer -> Picture

chessBoard n
    | n == 1    = black
    | n >= 1    = chessBoard (n - 1) `beside` rightRow `above` bottomRow
    where
    rightRow
        | odd n       = blackWhiteCol (n - 1)
        | otherwise   = whiteBlackCol (n - 1)
    bottomRow
        | odd n       = blackWhite n
        | otherwise   = whiteBlack n

-- Exercise 4.31

-- Euclid's algorithm
highestCF :: Integer -> Integer -> Integer
highestCF n m
    | m == 0  = n
    | m >=1   = highestCF m (n `rem` m)

-- Exercise 4.32

twoToTheN :: Integer -> Integer
twoToTheN  n
    | n == 0    = 1
    | n == 1    = 2
    | even n    = twoToTheN (n `div` 2) * twoToTheN (n `div` 2)
    | otherwise = twoToTheN (n `div` 2) * twoToTheN (n `div` 2) * 2

-- Exercise 4.33


allEqual :: Integer -> Integer -> Integer -> Bool
allEqual x y z
         | x == y && y == z     = True
         | otherwise            = False

testAllEqual1 = TestCase (assertEqual "for: allEqual 1 1 1" True  (allEqual 1 1 1 ))
testAllEqual2 = TestCase (assertEqual "for: allEqual 1 2 3" False (allEqual 1 2 3 ))
testAllEqual3 = TestCase (assertEqual "for: allEqual 1 1 3" False (allEqual 1 1 3 ))
testAllEqual4 = TestCase (assertEqual "for: allEqual 1 2 2" False (allEqual 1 2 2 ))
testAllEqual5 = TestCase (assertEqual "for: allEqual 1 2 1" False (allEqual 1 2 1 ))

testsAllEqual = TestList [testAllEqual1, testAllEqual2,
                          testAllEqual3, testAllEqual4, testAllEqual5]


-- Exercise 4.34

solution :: Integer -> Integer -> Integer -> Bool
solution x y z  = ((x + y + z) == (3 * z))

testSolution1 = TestCase (assertEqual "for: solution 1 1 1" True  (solution 1 1 1 ))
testSolution2 = TestCase (assertEqual "for: solution 1 2 3" False (solution 1 2 3 ))
testSolution3 = TestCase (assertEqual "for: solution 1 1 3" False (solution 1 1 3 ))
testSolution4 = TestCase (assertEqual "for: solution 1 2 2" False (solution 1 2 2 ))
testSolution5 = TestCase (assertEqual "for: solution 1 2 1" False (solution 1 2 1 ))

testsSolution = TestList [testSolution1, testSolution2,
                          testSolution3, testSolution4, testSolution5]
-- Exercise 4.35

allDifferent :: Integer -> Integer -> Integer -> Bool
allDifferent x y z
         | x /= y && y /= z && x /= z    = True
         | otherwise                     = False

testAllDifferent1 = TestCase (assertEqual "for: allDifferent 1 2 3" True  (allDifferent 1 2 3 ))
testAllDifferent2 = TestCase (assertEqual "for: allDifferent 1 1 1" False (allDifferent 1 1 1 ))
testAllDifferent3 = TestCase (assertEqual "for: allDifferent 1 1 3" False (allDifferent 1 1 3 ))
testAllDifferent4 = TestCase (assertEqual "for: allDifferent 1 2 2" False (allDifferent 1 2 2 ))
testAllDifferent5 = TestCase (assertEqual "for: allDifferent 1 2 1" False (allDifferent 1 2 1 ))

testsAllDifferent = TestList [testAllDifferent1, testAllDifferent2,
                              testAllDifferent3, testAllDifferent4, testAllDifferent5]
-- Exercise 4.36

attempt :: Integer -> Integer -> Integer -> Bool
attempt m n p = ( m /= n) && (n /= p)

testattempt1 = TestCase (assertEqual "for: attempt 1 2 3" True  (attempt 1 2 3 ))
testattempt2 = TestCase (assertEqual "for: attempt 1 1 1" False (attempt 1 1 1 ))
testattempt3 = TestCase (assertEqual "for: attempt 1 1 3" False (attempt 1 1 3 ))
testattempt4 = TestCase (assertEqual "for: attempt 1 2 2" False (attempt 1 2 2 ))
testattempt5 = TestCase (assertEqual "for: attempt 1 2 1" False (attempt 1 2 1 ))

testsattempt = TestList [testattempt1, testattempt2,
                         testattempt3, testattempt4, testattempt5]

-- As expected the last test fails

-- Exercise 4.37

averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = ( fromInteger x + fromInteger y + fromInteger z) / 3.0

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer

howManyAboveAverage x y z = (if (fromInteger x > averageThree x y z) then 1 else 0)
                          + (if (fromInteger y > averageThree x y z) then 1 else 0)
                          + (if (fromInteger z > averageThree x y z) then 1 else 0)

testHMAA1 = TestCase (assertEqual "for: HMAA 3 3 3" 0     (howManyAboveAverage 3 3 3 ))
testHMAA2 = TestCase (assertEqual "for: HMAA 3 4 5" 1     (howManyAboveAverage 3 4 5 ))
testHMAA3 = TestCase (assertEqual "for: HMAA 3 103 104" 2 (howManyAboveAverage 3 103 104 ))
testHMAA4 = TestCase (assertEqual "for: HMAA 5 4 3" 1     (howManyAboveAverage 5 4 3 ))
testHMAA5 = TestCase (assertEqual "for: HMAA 5 3 4" 1     (howManyAboveAverage 5 3 4 ))
testHMAA6 = TestCase (assertEqual "for: HMAA 3 5 4" 1     (howManyAboveAverage 3 5 4 ))

testsHMAA = TestList [testHMAA1, testHMAA2, testHMAA3, testHMAA4, testHMAA5, testHMAA6]

-- Exercise 4.38

-- Testing twoToTheN 

testTwoToTheN_1 = TestCase (assertEqual "for: twoToTheN 0" 1 (twoToTheN 0))
testTwoToTheN_2 = TestCase (assertEqual "for: twoToTheN 1" 2 (twoToTheN 1))
testTwoToTheN_3 = TestCase (assertEqual "for: twoToTheN 2" 4 (twoToTheN 2))
testTwoToTheN_4 = TestCase (assertEqual "for: twoToTheN 3" 8 (twoToTheN 3))

testsTwoToTheN = TestList [testTwoToTheN_1, testTwoToTheN_2, testTwoToTheN_3, testTwoToTheN_4]

-- Exercise 4.39

-- QuickCheck tests...


