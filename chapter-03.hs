import Test.QuickCheck

-- Exercise 3.1
original_xor True x = not x
original_xor False x = x

new_xor x y = (x && not y) || (not x && y)

-- Exercise 3.2
{- Box Diagram
                              +-----+
   x ------------------------>| &&  |-----------------+
   |                          +-----+                 |
   |                               ^                  |
   |                               |                  |
   |          +-----+              |                  |
   +--------->| not |-----------+  |                  |
              +-----+           |  |                  |
                                |  |                +-----+             
                                |  |                | ||  |             
                                |  |                +-----+             
              +-----+           |  |                  |
   + -------->| not |-----------+--+                  |
   |          +-----+           |                     |
   |                            |                     |         
   |                            v                     |         
   |                          +-----+                 |
   y ------------------------>| &&  |-----------------+
                              +-----+                 
-}

-- Exercise 3.3
literal_xor True True   = False
literal_xor True False  = True
literal_xor False True  = True
literal_xor False False = False

-- Exercise 3.4
my_and True True    = True
my_and True False   = False
my_and False True   = False
my_and False False  = False

-- Exercise 3.5
nAnd_1 True True    = False
nAnd_1 True False   = True
nAnd_1 False True   = True
nAnd_1 False False  = True

nAnd_2 x y = not (x && y)

{-
Diagram

    x ---------+
               |
               v
            +-----+       +-----+
            | &&  |------>| not |------>
            +-----+       +-----+
               ^
               |
    y ---------+
-}


-- Exercise 3.6
{- Line-by-line calculations of 

nAnd_2 True True
~   not (True && True)
~   not (True)
~   False

nAnd_2 True False
~   not (True && False)
~   not (False)
~   True

-}

-- Exercise 3.7
-- Quickcheck of the functions above

prop_xors x y = (original_xor x y == new_xor x y) &&
                (original_xor x y == literal_xor x y)


-- Exercise 3.8

mystery :: Integer -> Integer -> Integer -> Bool
mystery m n p = not ((m == n) && (n == p))

{-
    ghci> mystery 3 4 5
    True
    ghci> mystery 3 3 3
    False
    ghci> mystery 3 3 1
    True

    mystery tells us that the 3 numbers are different.
-}

-- Exercise 3.9

threeDifferent :: Integer -> Integer -> Integer -> Bool
threeDifferent  m n p = (m /= n) && (m /= p) && (n /= p) 

{-
    ghci> threeDifferent 3 4 5
    True
    ghci> threeDifferent 3 4 3
    False
    ghci> threeDifferent 5 4 5
    False
    ghci> 
-}

-- Exercise 3.10


threeEqual :: Integer -> Integer -> Integer -> Bool
threeEqual m n p = (m == n) && (n == p)

fourDifferent :: Integer -> Integer -> Integer -> Integer -> Bool
fourDifferent m n p q = threeDifferent m n p && threeDifferent n p q && threeDifferent m p q



-- fourDifferent' m n p q = not threeEqual

-- Exercise 3.11

-- Exercise 3.12

-- Started out with this:

prop_threeDifferent m n p =    (threeDifferent m n p == threeDifferent n p m)
                            && (threeDifferent m n p == threeDifferent p m n)
                            && (threeDifferent m n p == threeDifferent m p n)
                            && (threeDifferent m n p == threeDifferent n m p)
                            && (threeDifferent m n p == threeDifferent p n m)
                            && (threeDifferent m n n == False)
                            && (threeDifferent m m n == False)
                            && (threeDifferent m m m == False)
                            && (threeDifferent m (m + 1) (m + 2) == True)

prop_fourDifferent m n p q =   (fourDifferent m n p q == fourDifferent m n q p)
                            && (fourDifferent m n p q == fourDifferent m p n q)
                            -- ... there are 24 of these checks based on combinations
                            -- of m n p q
                            && (fourDifferent m n n n == False)
                            && (fourDifferent m m n n == False)
                            && (fourDifferent m m m n == False)
                            && (fourDifferent m m m m == False)
                            && (fourDifferent m (m + 1) (m + 2) (m + 3) == True)


-- Exercise 3.13

{-
    ghci> max (3-2) (3*8)
    24
    ghci> maxTree (4+5) (2*6) (100 `div` 7)
    14
-}

-- Exercise 3.14

min' :: Int -> Int -> Int

min' x y
        | x < y = x
        | otherwise = y

minThree :: Int -> Int -> Int -> Int

minThree x y z
        | x <= y && x <= z = x
        | y <= z = y
        | otherwise = z

-- Exercise 3.15

prop_min' x y =  (min' x y == min' y x)
              && (min' x (x + 1) == x)
              && (min' (x + 1) x == x)

prop_minThree x y z =  (minThree x y z == minThree x z y)
                    && (minThree x y z == minThree y x z)
                    && (minThree x y z == minThree z x y)
                    && (minThree x y z == minThree z y x)
                    && (minThree x (x + 1) (x + 2) == x)

-- Exercise 3.16

offset :: Int
offset = fromEnum 'A' - fromEnum 'a'

toUpper :: Char -> Char
toUpper ch = toEnum(fromEnum ch + offset)

isLower :: Char -> Bool
isLower ch = 'a' <= ch && ch <= 'z' 

smallToCapitals :: Char -> Char

smallToCapitals ch
        | isLower ch = toUpper ch
        | otherwise = ch


-- Exercise 3.17

isDigit :: Char -> Bool
isDigit ch = ('0' <= ch) && (ch <= '9')

charToNum :: Char -> Int

charToNum ch
        | isDigit ch = fromEnum ch - fromEnum '0'
        | otherwise = 0

-- Exercise 3.18

onThreeLines :: String -> String -> String -> String
onThreeLines a b c = a ++ "\n" ++ b ++ "\n" ++ c ++ "\n"

-- Exercise 3.19

romanDigit :: Char -> String
romanDigit ch
        | ch == '1' = "I"
        | ch == '2' = "II"
        | ch == '3' = "III"
        | ch == '4' = "IV"
        | ch == '5' = "V"
        | ch == '6' = "VI"
        | ch == '7' = "VII"
        | ch == '8' = "VIII"
        | ch == '9' = "IX"
        | otherwise = "???"


-- Exercise 3.20

averageThree :: Integer -> Integer -> Integer -> Float
averageThree x y z = ( fromInteger x + fromInteger y + fromInteger z) / 3.0

howManyAboveAverage :: Integer -> Integer -> Integer -> Integer
howManyAboveAverage x y z = (if (fromInteger x > averageThree x y z) then 1 else 0)
                          + (if (fromInteger y > averageThree x y z) then 1 else 0)
                          + (if (fromInteger z > averageThree x y z) then 1 else 0)

-- Exercise 3.21
-- QuickChecks

prop_averageThree x y z = (averageThree x x x == fromInteger(x))
                       && (averageThree x (x + 1) (x + 2) == fromInteger(x + 1))
                       && (averageThree x y z == averageThree x z y)
                       && (averageThree x y z == averageThree y x z)
                       && (averageThree x y z == averageThree y z x)
                       && (averageThree x y z == averageThree z y x)

prop_hasManyAboveAverage x y z = (howManyAboveAverage x x x == 0)
                              && (howManyAboveAverage x (x + 1) (x + 2) == 1)
                              && (howManyAboveAverage x (x + 100) (x + 101) == 2)
                              && (howManyAboveAverage x y z == howManyAboveAverage x z y)
                              && (howManyAboveAverage x y z == howManyAboveAverage y x z)
                              && (howManyAboveAverage x y z == howManyAboveAverage y z x)
                              && (howManyAboveAverage x y z == howManyAboveAverage z y x)

-- Exercise 3.22

numberNDroots :: Float -> Float -> Float -> Integer

numberNDroots a b c
              | b * b > 4.0 * a * c = 2
              | b * b == 4.0 * a * c = 1
              | b * b < 4.0 * a * c = 0

-- Exercise 3.23

numberRoots :: Float -> Float -> Float -> Integer
numberRoots a b c
            | a /= 0 = numberNDroots a b c
            | otherwise = if b /= 0.0 then 1
                          else if b == 0.0 && c /= 0.0 then 0
                          else 3
                            
-- Exercise 3.24

smallerRoot:: Float -> Float -> Float -> Float
smallerRoot a b c = (-b - sqrt(b*b - 4.0 * a * c))/(2*a)

largerRoot:: Float -> Float -> Float -> Float
largerRoot a b c = (-b + sqrt(b*b - 4.0 * a * c))/(2*a)


-- Exercise 3.25
-- Quickchecks

prop_roots a b c = (smallerRoot a b c <= largerRoot a b c)

-- TODO: doesn't work for imaginary roots

-- Exercise 3.26
-- Rewrite solutions using recommended layout

-- Exercise 3.27

funny x = x * x
peculiar y = y

-- removing the space fixes the "parse error on input"