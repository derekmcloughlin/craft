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

testPic3 :: Picture
testPic3 = [ "XXXXXXXXXXXXXX",
             "XXXXXXXXXXXXXX",
             "XXXXXXXXXXXXXX",
             "XXXXXXXXXXXXXX",
             "XXXXXXXXXXXXXX",
             "XXXXXXXXXXXXXX",
             "XXXXXXXXXXXXXX",
             "XXXXXXXXXXXXXX",
             "XXXXXXXXXXXXXX",
             "XXXXXXXXXXXXXX"]

blankPic :: Int -> Int -> Picture
blankPic w h = replicate h $ replicate w '.'

padPicture :: Picture -> Int -> Int -> Picture

padPicture pic w h
    | width pic >= w && height pic >= h  = pic
    | otherwise = pic `beside`  blankPic (w - width pic) (height pic)
                  `above`
                  blankPic w1 (h - height pic)
    where w1 = max w (width pic)
          h1 = max h (height pic)

beside2 :: Picture -> Picture -> Picture

beside2 pic1 pic2 = (padPicture pic1 w h) `beside` (padPicture pic2 w h)
    where w = min (width pic1) (width pic2)
          h = max (height pic1) (height pic2)

above2 :: Picture -> Picture -> Picture

above2 pic1 pic2 = (padPicture pic1 w h) `above` (padPicture pic2 w h)
    where w = max (width pic1) (width pic2)
          h = min (height pic1) (height pic2)

-- Exercise 6.18

weirdPic :: Picture
weirdPic = [ "XXXXX",
             "YYYYYYYYY",
             "ZZ",
             "W",
             "PPPPP"]

padLine :: String -> Int -> String
padLine s n
    | n <= length s      = s
    | otherwise          = s ++ replicate (n - length s) '.'

maxWidth :: Picture -> Int
maxWidth pic
    | pic == []         = 0
    | otherwise         = max (length $ head pic) (maxWidth $ tail pic)

makeRectangular :: Picture -> Picture
makeRectangular pic = [ padLine line n | line <- pic]
                      where n = maxWidth pic

beside3 :: Picture -> Picture -> Picture
beside3 pic1 pic2 = (makeRectangular pic1) `beside2` (makeRectangular pic2)

above3 :: Picture -> Picture -> Picture
above3 pic1 pic2 = (makeRectangular pic1) `above2` (makeRectangular pic2)

-- Exercise 6.19

type Picture2 = [[Bool]]

horse2 :: Picture2

horse2 = [[False, False, False, False, False, False, False, True , True , False, False, False],
          [False, False, False, False, False, True , True , False, False, True , False, False],
          [False, False, False, True , True , False, False, False, False, False, True , False],
          [False, False, True , False, False, False, False, False, False, False, True , False],
          [False, False, True , False, False, False, True , False, False, False, True , False],
          [False, False, True , False, False, False, True , True , True , False, True , False],
          [False, True , False, False, False, False, True , False, False, True , True , False],
          [False, False, True , False, False, False, True , False, False, False, False, False],
          [False, False, False, True , False, False, False, True , False, False, False, False],
          [False, False, False, False, True , False, False, True , False, False, False, False],
          [False, False, False, False, False, True , False, True , False, False, False, False],
          [False, False, False, False, False, False, True , True , False, False, False, False]]

printChar :: Bool -> Char
printChar b
    | b == True = '#'
    | otherwise = '.'

printLine :: [Bool] -> String
printLine line = [printChar b | b <- line] ++ "\n"

printPicture2 :: Picture2 -> IO ()
printPicture2 pic = putStr $ concat [printLine line | line <- pic]
          
-- Exercise 6.20

-- Picture as a list of columns
type PictureCol = [String]

horse3 :: PictureCol
horse3 = [ "............",
           ".....#......",
           "....#.###...",
           "...#.....#..",
           "..#......#..",
           ".#........#.",
           "#...####..#.",
           "####..#....#",
           "......#....#",
           ".....#....#.",
           ".....#####..",
           "............"]

-- This makes it very tricky to do normal operations like print.
-- Effectively we've pivoted or rotated the horse.
-- The function 'above' is really like 'beside' and vice versa.

-- Exercise 6.21

-- For example: the code here is very similar to rotate90
printPictureCol :: PictureCol -> IO ()
printPictureCol pic = putStr $ onSeparateLines $
                             flipH [ [xs !! i | xs <- pic] | i <- [0 .. length (pic!!0) - 1]]
                   where onSeparateLines xs = [ch | x <- xs, ch <- x ++ "\n"]

-- Exercise 6.22

type Picture4 = String
horse4 :: Picture4

horse4 = ".......##...\n.....##..#..\n...##.....#.\n..#.......#.\n..#...#...#.\n..#...###.#.\n.#....#..##.\n..#...#.....\n...#...#....\n....#..#....\n.....#.#....\n......##...."

-- This is easy
printPicture4 :: Picture4 -> IO ()
printPicture4 pic = putStr pic

-- Everything else is much harder. Effectively you'd split the strings anyway to
-- make them easier to deal with.

-- Exercise 6.23

-- RLE

type PictureRLE = [[(Int, Char)]]

horse5 :: PictureRLE

horse5 = [
           [(7, '.'), (2, '#'), (3, '.')],
           [(5, '.'), (2, '#'), (2, '.'), (1, '#'), (2, '.')],
           [(3, '.'), (2, '#'), (5, '.'), (1, '#'), (1, '.')],
           [(2, '.'), (1, '#'), (7, '.'), (1, '#'), (1, '.')],
           [(2, '.'), (1, '#'), (3, '.'), (1, '#'), (3, '.'), (1, '#'), (1, '.')],
           [(2, '.'), (1, '#'), (3, '.'), (3, '#'), (1, '.'), (1, '#'), (1, '.')],
           [(1, '.'), (1, '#'), (4, '.'), (1, '#'), (2, '.'), (2, '#'), (1, '.')],
           [(2, '.'), (1, '#'), (3, '.'), (1, '#'), (5, '.')],
           [(3, '.'), (1, '#'), (3, '.'), (1, '#'), (4, '.')],
           [(4, '.'), (1, '#'), (2, '.'), (1, '#'), (4, '.')],
           [(6, '.'), (2, '#'), (4, '.')]
         ]
           

printPictureRLE :: PictureRLE -> IO ()

printPictureRLE pic = putStr $ concat [concat [ replicate n ch | (n, ch) <- line] ++ "\n" | line <- pic]

-- flipV and flipH are exactly the same
flipVRLE :: PictureRLE -> PictureRLE
flipVRLE pic = map reverse pic

flipHRLE :: PictureRLE -> PictureRLE
flipHRLE pic = reverse pic

-- 'above' is the same as the non-RLE version

aboveRLE :: PictureRLE -> PictureRLE -> PictureRLE
aboveRLE = (++)

-- 'beside *can* be the same as the non-RLE version
-- However it's not optimised - see exercise 6.24

besideRLE :: PictureRLE -> PictureRLE -> PictureRLE

besideRLE = zipWith (++)

-- Exercise 6.24

-- compact the RLE
type LineRLE = [(Int, Char)]

testLineRLE :: LineRLE
testLineRLE = [(3, '.'), (4, '#'), (2, '#'), (7, '.')]

stringToRLE :: String -> LineRLE
stringToRLE s = str_acc [] s

-- TODO: There's probably a better way to write this using an apply function.
str_acc :: [Char] -> [Char] -> LineRLE
str_acc chs xs
    | xs == []              = [(length chs, head chs)] 
    | chs == []             = str_acc [head xs] (tail xs) 
    | head xs == head chs   = str_acc (chs ++ [head xs]) (tail xs) 
    | otherwise             = [(length chs, head chs)] ++ (str_acc [] xs)

rleToString :: LineRLE -> String
rleToString line = concat [ replicate n ch | (n, ch) <- line]

compactLineRLE :: LineRLE -> LineRLE
compactLineRLE line = stringToRLE $ rleToString line 

compactPictureRLE :: PictureRLE -> PictureRLE
compactPictureRLE pic = [compactLineRLE line | line <- pic]

-- TODO: Could use a similar approach in str_acc to compact without
-- having to go through intermediate string steps

-- This leads us to a compacted 'beside'

besideRLE' :: PictureRLE -> PictureRLE -> PictureRLE

besideRLE' pic1 pic2 = compactPictureRLE $ zipWith (++) pic1 pic2

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



