import Pictures
import Test.QuickCheck
import Test.HUnit

-- Exercise 6.1

{-

snd :: (a, b) -> b
snd (x, y) = y

sing :: a -> [a]
sing x = [x]

-}

-- Exercise 6.2

{-
    somefunc :: [[a]] -> [[a]]

    Not the most generic, because it needs it's input to be an array of arrays

-}

-- Exercise 6.3

shift :: ((a, a), a) -> (a, (a, a))

shift ((x, y), z) = (x, (y, z))

-- Exercise 6.4

-- NOTE: because we're importing Pictures I'm renaming the following functions

superimposeChar' :: Char -> Char -> Char

superimposeChar' ch1 ch2
    | ch1 == '.' && ch2 == '.'   = '.'
    | otherwise                  = '#'

-- Exercise 6.5

superimposeLine' :: [Char] -> [Char] -> [Char]

superimposeLine' line1 line2 = [superimposeChar' ch1 ch2 | (ch1, ch2) <- zip line1 line2]

-- Exercise 6.6

superimpose' :: Picture -> Picture -> Picture

superimpose' pic1 pic2 = [superimposeLine' line1 line2 | (line1, line2) <- zip pic1 pic2]

-- Exercise 6.7

printPicture' :: Picture -> IO ()

printPicture' pic = putStr $ onSeparateLines pic
                   where onSeparateLines xs = [ch | x <- xs, ch <- x ++ "\n"]

-- Exercise 6.8

-- Test picture

testPic :: Picture
testPic = [ "a.X.b",
            ".....",
            ".....",
            "Q...Z",
            ".....",
            ".....",
            "d.P.c"]

rotate90 :: Picture -> Picture

rotate90 pic = flipV [ [xs !! i | xs <- pic] | i <- [0 .. length (pic!!0) - 1]]

-- Exercise 6.9


rotate90anti :: Picture -> Picture

rotate90anti pic = rotate90 $ rotate90 $ rotate90 pic

-- Exercise 6.10

-- Helper
scaleLineH :: String -> Int -> String
scaleLineH line n = concat [ replicate n ch | ch <- line] 

-- Horizontal scaling
scaleH :: Picture -> Int -> Picture
scaleH pic n = [scaleLineH line n | line <- pic]

scaleV pic n = concat [ replicate n line | line <- pic]

scale pic n = scaleV (scaleH pic n) n

-- A madder way to do all this is to use the scaleH and rotate90 functions:

scale' :: Picture -> Int -> Picture

scale' pic n = rotate90 $ rotate90 $ rotate90 $ scaleH (rotate90 $ scaleH pic n) n


-- Exercise 6.11

testPic2 :: Picture

testPic2 = [ "A.x.B",
             ".....",
             ".....",
             "q...z",
             ".....",
             ".....",
             "D.p.C"]

prop_AboveFlipH_Fails :: Picture -> Picture -> Bool

prop_AboveFlipH_Fails pic1 pic2 =
                      flipH (pic1 `above` pic2) == (flipH pic1) `above` (flipH pic2)

prop_AboveFlipH_Works pic1 pic2 =
                      flipH (pic1 `above` pic2) == (flipH pic2) `above` (flipH pic1)

-- Exercise 6.12

prop_BesideFlipV_Works :: Picture -> Picture -> Bool
                    
prop_BesideFlipV_Works pic1 pic2 =
                 flipV (pic1 `beside` pic2) == (flipV pic2) `beside` (flipV pic1)

prop_BesideFlipH_Works :: Picture -> Picture -> Bool
                    
prop_BesideFlipH_Works pic1 pic2 =
                 flipH (pic1 `beside` pic2) == (flipH pic1) `beside` (flipH pic2)

-- Exercise 6.13

prop_FourPics :: Picture -> Bool

prop_FourPics pic =
              (pic `above` pic) `beside` (pic `above` pic) ==
              (pic `beside` pic) `above` (pic `beside` pic)

-- Exercise 6.14

{-
    The following test will cause quickCheck to give up, as a lot of it's generated
    test cases won't match the conditions.

    See http://stackoverflow.com/questions/12884927/conditional-quickcheck-properties

-}

prop_rotate90  :: Picture -> Property
prop_rotate90 pic =
    (rectangular pic && width pic > 0)
    ==>
    pic == (rotate90 $ rotate90 $ rotate90 $ rotate90 pic)

-- Exercise 6.15

-- This only works if the picture is monochrome - only has '.' or '#'
prop_InvertColour' :: Picture -> Bool
prop_InvertColour' pic =
                   pic == (invertColour $ invertColour pic)

-- Exercise 6.16

nonRectangularPic :: Picture
nonRectangularPic  = ["...", "###", "."]

rectangular' :: Picture -> Bool

rectangular' pic = and [(length line == n) | line <- pic]
                  where n = width pic

propAboveBeside3Correct w e =
    (rectangular w && rectangular e && height w == height e)
    ==>
    (w `beside` e) `above` (w `beside` e)
    ==
    (w `above` w) `beside` (e `above` e)

propAboveBeside4 w e =
    (rectangular w && rectangular e && height w == height e)
    ==>
    (w `beside` w) `above` (e `beside` e)
    ==
    (w `above` e) `beside` (w `above` e)

    
-- Exercise 6.17



-- Exercise 6.18



-- Exercise 6.19



-- Exercise 6.20



-- Exercise 6.21



-- Exercise 6.22



-- Exercise 6.23



-- Exercise 6.24



-- Exercise 6.25



-- Exercise 6.26



-- Exercise 6.27



-- Exercise 6.28



-- Exercise 6.29



-- Exercise 6.30



-- Exercise 6.31



-- Exercise 6.32



-- Exercise 6.33



-- Exercise 6.34



