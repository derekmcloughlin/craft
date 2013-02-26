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


-- Exercise 5.6


-- Exercise 5.7


-- Exercise 5.8


-- Exercise 5.9


