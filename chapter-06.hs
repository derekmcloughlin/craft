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

-- Quickcheck properties
-- They should all be the same?


-- Exercise 6.26

type PictureRLE2 = (Int, [(Int, Char)])

-- Exercise 6.27

-- TODO

-- Exercise 6.28

-- TODO

-- Exercise 6.29

type Position = (Int, Int)

type Image = (Picture, Position)

makeImage :: Picture -> Position -> Image

makeImage pic pos = (pic, pos)

-- Exercise 6.30

changePosition :: Image -> Position -> Image

changePosition (pic, pos) newpos = (pic, newpos)

-- Exercise 6.31

moveImage :: Image -> Int -> Int -> Image

moveImage (pic, (x, y)) xMove yMove = (pic, (x + xMove, y + yMove))

-- Exercise 6.32

printImage :: Image -> IO ()

printImage (pic, (x, y)) = printPicture pic 

-- x and y?

-- Exercise 6.33

flipImageH :: Image -> Image

-- TODO
flipImageH (pic, (x, y)) = (pic, (x, y))

-- Exercise 6.34

-- TODO

-- Exercise 6.35

-- TODO

-- Exercise 6.36

-- TODO

-- Exercise 6.37

-- TODO

-- Exercise 6.38

-- TODO

-- Exercise 6.39

type Name = String
type Price = Int
type BillType = [(Name, Price)]

formatPence :: Price -> String

formatPence n = (show pounds) ++ "." ++ (pad0 pence ) 
            where pounds = n `div` 100
                  pence = n `mod` 100
                  pad0 p = if p < 10 then
                              "0" ++ show (p)
                           else
                              show p

-- Exercise 6.40

formatLine :: (Name, Price) -> String

formatLine (name, price) = name ++ replicate n '.' ++ priceStr ++ "\n"
                           where n = 30 - (length name) - (length priceStr)
                                 priceStr = formatPence price


-- Exercise 6.41

-- Some test lines

testLines :: [(Name, Price)]
testLines = [ ("Dry Sherry, 1lt", 540),
              ("Wet Sherry, 2lt", 1234),
              ("Pink Lemonade, 3lt", 403)]

formatLines :: [(Name, Price)] -> String

formatLines lines = concat [formatLine line | line <- lines]

-- Exercise 6.42

makeTotal :: BillType -> Price

makeTotal bill = sum [price | (_, price) <- bill]


-- Exercise 6.43

formatTotal :: Price -> String

formatTotal price = "\n" ++ formatLine ("Total", price)

-- Exercise 6.44

centre :: Int -> String -> String
centre w s = replicate left '.' ++ s ++ replicate right '.' 
             where 
             n = length s
             left = (w - n) `div` 2
             right = w - (n + left)

formatBill :: BillType -> String

formatBill bill = header ++ (formatLines bill) ++ (formatTotal $ makeTotal bill)
                  where header = "\n\n" ++ (centre 30 "Haskell Stores") ++ "\n\n"

-- Exercise 6.45

type Database = [(Barcode, Name, Price)]

type Barcode = String

{- Note: it's important to give the testDB its type.
   If you just have the data without the type then
   the type of testDB is
   [([Char], [Char], Integer)]
   and you'll get a type error in the "look" function.
-}

testDB :: Database
testDB = [
           ("1111", "Dry Sherry, 1Lt", 540),
           ("2222", "Wet Sherry ,2Lt", 1234),
           ("3333", "Pink Lemonade, 3Lt", 403)
         ]

look :: Database -> Barcode -> (Name, Price)

look db  barcode = head $ [(name, price) | (foundBarcode, name, price) <- db, foundBarcode == barcode]
                          ++ [("Unknown Item", 0)]

-- Exercise 6.46

lookup' :: Barcode -> (Name, Price)
lookup' barcode = look testDB barcode

-- Exercise 6.47

type TillType = [Barcode]

testTill :: TillType
testTill = ["1111", "2222", "3333", "1111", "4444"]

makeBill :: TillType -> BillType

{-  I originally had this:

    makeBill tillInfo = [(name, price) | barcode <- tillInfo, (name, price) <- lookup' barcode]

    However, I was getting type errors on the return of lookup':

        Couldn't match expected type `[t0]'
                    with actual type `(Name, Price)'
        In the return type of a call of lookup'
        In the expression: lookup' barcode
        In a stmt of a list comprehension: (name, price) <- lookup' barcode

    This is because <- is expecting an array of things, not just a single thing.
-}

makeBill tillInfo = [lookup' barcode | barcode <- tillInfo]

-- Exercise 6.48

makeDiscount :: BillType -> Price

makeDiscount bill
    | numSherries > 1 = 100
    | otherwise = 0
    where numSherries = sum [ 1 | (name, _) <- bill, name == "Dry Sherry, 1Lt"]

formatDiscount :: Price -> String
formatDiscount price
    | price > 0 = "\n" ++ formatLine ("Discount", price)
    | otherwise = ""

formatBill' :: BillType -> String

formatBill' bill = header ++
                   (formatLines bill) ++
                   (formatDiscount $ makeDiscount bill) ++
                   (formatTotal $ makeTotal bill)
                   where header = "\n\n" ++ (centre 30 "Haskell Stores") ++ "\n\n"

-- Exercise 6.49

addBarcode :: Database -> Barcode -> (Name, Price) -> Database

-- Naive version - just add
-- addBarCode db barcode (name, price) = [(barcode, name, price)] ++ db


addBarcode db barcode (name, price) = [(barcode, name, price)] ++
                                      removeBarcode db barcode

removeBarcode :: Database -> Barcode -> Database

removeBarcode db barcode = [(bc, name, price) | (bc, name, price) <- db, bc /= barcode]


-- Exercise 6.50


{-  Done as a change to the formatLines function:
    formatLines lines = concat [formatLine (name, price) | (name, price) <- lines, name /= "Unknown Item"]
-}


-- Exercise 6.51

-- Testing with QuickCheck

-- Exercise 6.52

-- Project - harder


-- Exercise 6.53

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Show, Eq, Ord, Enum)

-- Note: the ordering here is increased value           
data Value = Two   |
             Three |
             Four  |
             Five  |
             Six   |
             Seven |
             Eight |
             Nine  |
             Ten   |
             Jack  |
             Queen |
             King  |
             Ace
             deriving (Show, Eq, Ord, Enum)

type Card = (Suit, Value)

type Deck = [Card]

-- Really only have 52 distinct entries in a deck. We could hard-code these
-- or use a list comprehension to do it.
fullDeck :: Deck            
fullDeck = [(suit, value) | suit <- [Clubs .. Spades], value <- [Two .. Ace]]

-- Exercise 6.54

{-  Rationale for choices in previous exercise

    A data type is better than strings in this case.
    The suits are often ordered in games like Bridge.
    The values are a finite set and have an ordering.

-}

-- Exercise 6.55

data Player = North | East | West | South
              deriving (Show, Eq)

-- Exercise 6.56

type Trick = [(Player, Card)]

-- The first item in the trick is the lead
testTrick :: Trick
testTrick = [(North, (Clubs, Seven)),
             (East,  (Spades, Ten)),
             (South, (Hearts, Two)),
             (West,  (Clubs, Nine))]

-- Exercise 6.57

-- No Trumps
-- The first suit lead is effectly the trumps
-- Look to see who played the highest card in that suit
-- I've redefined this in terms of the more generic winT

winNT :: Trick -> Player
            
winNT trick = winT trumpSuit trick
    where trumpSuit = suitLead trick

-- Exercise 6.58

-- Trumps - If there were trumps played, look for the highest one
--          Otherwise it's effectively a no-trumps game - so the trump is the first suit lead.
winT :: Suit -> Trick -> Player

winT trumpSuit trick = whoPlayed
     where hasTrumps = length [player | (player, (suit, value)) <- trick, suit == trumpSuit] > 0
           realTrumpSuit = if hasTrumps then trumpSuit else suitLead trick
           maxValue = maximum [value | (_, (suit, value)) <- trick, suit == realTrumpSuit]
           whoPlayed = head [player | (player, (suit, value)) <- trick, suit == realTrumpSuit, value == maxValue]

-- Exercise 6.59

type Hand = [Card]

northsHand :: Hand

northsHand = [(Spades, Ace), (Spades, Two), (Hearts, King), (Hearts, Seven), (Diamonds, Three)]

-- Exercise 6.60

type Hands = [(Player, Hand)]

allHands :: Hands

allHands = [ (North, [(Spades, Ace), (Spades, Two),
                      (Hearts, King), (Hearts, Seven),
                      (Diamonds, Three)]),
             (East,  [(Spades, Three),
                      (Hearts, Jack),
                      (Diamonds, Seven), (Diamonds, Two),
                      (Clubs, Nine)]),
             (South, [(Spades, King), (Spades, Queen), (Spades, Jack),
                      (Hearts, Two),
                      (Clubs, Ace)]),
             (West,  [(Spades, Six),
                      (Diamonds, Nine), (Diamonds, Five),
                      (Clubs, King), (Clubs, Eight)])
           ]  

-- Exercise 6.61

-- The following are possible tricks coming from allHands above

trick1, trick2 :: Trick

trick1 = [(North, (Spades, Ace)),
          (East,  (Spades, Three)),
          (South, (Spades, Jack)),
          (West,  (Spades, Six))
         ]

trick2 = [(East,  (Clubs, Nine)),
          (South, (Clubs, Ace)),
          (West,  (Clubs, Eight)),
          (North, (Diamonds, Three))  -- Not holding clubs so it's OK.
         ]

-- The following are invalid tricks          

-- Not possible
trick3 = [(North, (Clubs, Seven)),    -- North doesn't have 7 of Clubs
          (East,  (Spades, Ten)),
          (South, (Hearts, Two)),
          (West,  (Clubs, Nine))
         ]

-- Not legal - 
trick4 = [(North, (Spades, Ace)),
          (East,  (Spades, Three)),
          (South, (Spades, Jack)),
          (West,  (Diamonds, Five))   -- West has a spade so should lead it
         ]

checkPlayPossible :: Hands -> Trick -> Bool

-- 1st part - is it possible - i.e. is the card the player played in their hand?

checkPlayPossible hands trick = and [ isInHand hands player card | (player, card) <- trick]

isInHand :: Hands -> Player -> Card -> Bool

isInHand hands player card = length (playerHasCard hands player card) > 0
    where playerHasCard hands player card = [p | (p, hand) <- hands, c <- hand, p == player, c == card]


-- 2nd part - is it legal - did the player follow suit of lead?

checkPlayLegal :: Hands -> Trick -> Bool
    
checkPlayLegal hands trick = and [followedSuitIfPossible hands trumpSuit player card | (player, card) <- trick]
    where trumpSuit = suitLead trick

followedSuitIfPossible :: Hands -> Suit -> Player -> Card -> Bool
followedSuitIfPossible  hands suit player playedCard
    | suit == playedSuit                      = True
    | notHolding hands player suit == True    = True
    | otherwise                               = False
    where playedSuit = fst playedCard


notHolding :: Hands -> Player -> Suit -> Bool
notHolding hands player suit = length [s | (p, hand) <- hands, (s, v) <- hand, p == player, s == suit] == 0

suitLead :: Trick -> Suit
suitLead trick = fst $ snd $ head trick


checkPlay :: Hands -> Trick -> Bool
checkPlay hands trick = checkPlayPossible hands trick && checkPlayLegal hands trick


-- Exercise 6.62

trick5 = [
          (East,  (Diamonds, Two)),
          (South, (Spades, Jack)),
          (West,  (Spades, Six)),
          (North, (Spades, Ace))
         ]

tricks = [trick1, trick2, trick3, trick4, trick5]

data Team = NorthSouth | EastWest
            deriving (Show, Eq)

whichTeam :: Player -> Team
whichTeam North = NorthSouth
whichTeam South = NorthSouth
whichTeam East  = EastWest
whichTeam West  = EastWest

-- Note: this does *no* checking that the tricks are possible or legal
-- Also note that the number of tricks should be odd - e.g. 13 in Bridge.

winnerNT :: [Trick] -> (Team, Int)

winnerNT tricks
    | northSouthTricks > eastWestTricks   = (NorthSouth, northSouthTricks)
    | otherwise                           = (EastWest, eastWestTricks)
    where winningTeams = [whichTeam $ winNT trick | trick <- tricks]
          northSouthTricks = length [team | team <- winningTeams, team == NorthSouth]
          eastWestTricks = length [team | team <- winningTeams, team == EastWest]
          


-- This is the same function with a suit specified
winnerT :: Suit -> [Trick] -> (Team, Int)

winnerT suit tricks
    | northSouthTricks > eastWestTricks   = (NorthSouth, northSouthTricks)
    | otherwise                           = (EastWest, eastWestTricks)
    where winningTeams = [whichTeam $ winT suit trick | trick <- tricks]
          northSouthTricks = length [team | team <- winningTeams, team == NorthSouth]
          eastWestTricks = length [team | team <- winningTeams, team == EastWest]

-- Exercise 6.63

-- Call it something different from 'checkPlay' above.
-- Also, the book definition is missing the initial set of hands.

checkPlays :: Hands -> [Trick] -> Bool

-- Can recursively do this. After each trick played, the trick cards are removed from the
-- "hands" and the next trick is used.

checkPlays hands tricks
    | hands  == [] = True
    | tricks == [] = True
    | otherwise   = checkPlay hands currentTrick &&
                    checkPlays (removeCardsFromHands hands currentTrick) (tail tricks)
    where currentTrick = head tricks

removeCardsFromHands :: Hands -> Trick -> Hands
removeCardsFromHands hands trick = [(player, removeCardFromHand hand card) |
                           (player, hand) <- hands, (player2, card) <- trick, player == player2]

removeCardFromHand :: Hand -> Card -> Hand
removeCardFromHand hand card = [c | c <- hand, c /= card]

-- Test hands and tricks

-- This is the 1st hand that appears in Andrew Robson's book on bridge.

testHands :: Hands

testHands = [ (North, [
                 (Spades, Four), (Spades, Two),
                 (Hearts, Ace), (Hearts, Queen), (Hearts, Seven), (Hearts, Four),
                 (Diamonds, Ace), (Diamonds, Seven), (Diamonds, Five), (Diamonds, Three), (Diamonds, Two), 
                 (Clubs, Six), (Clubs, Three) 
              ]),
             (East,  [
                 (Spades, Five), (Spades, Three),
                 (Hearts, Eight), (Hearts, Six), (Hearts, Five),
                 (Diamonds, King), (Diamonds, Queen), (Diamonds, Ten),
                 (Clubs, Queen), (Clubs, Jack), (Clubs, Five), (Clubs, Four), (Clubs, Two)
              ]),
             (South, [
                 (Spades, Ace), (Spades, King), (Spades, Ten), (Spades, Nine), (Spades, Seven),
                 (Hearts, King), (Hearts, Jack), (Hearts, Ten), (Hearts, Nine),
                 (Diamonds, Six),
                 (Clubs, Ten), (Clubs, Eight), (Clubs, Seven)
              ]),
             (West,  [
                 (Spades, Queen), (Spades, Jack), (Spades, Eight), (Spades, Six), 
                 (Hearts, Three), (Hearts, Two),
                 (Diamonds, Jack), (Diamonds, Nine), (Diamonds, Eight), (Diamonds, Four),
                 (Clubs, Ace), (Clubs, King), (Clubs, Nine)
              ])
           ]

-- These are the actual tricks played in the game = 4♥ by South

testTricks = [ 
    -- E/W
    [(West,  (Clubs, Ace)),     (North, (Clubs, Six)),    (East,  (Clubs, Two)),    (South, (Clubs, Eight))],
    [(West,  (Clubs, King)),    (North, (Clubs, Three)),  (East,  (Clubs, Four)),   (South, (Clubs, Ten))],

    -- N/S
    [(West,  (Diamonds, Four)), (North, (Diamonds, Ace)),  (East,  (Diamonds, Ten)),   (South, (Diamonds, Six))],
    [(North, (Spades, Two)),    (East,  (Spades, Five)),   (South, (Spades, Ace)),     (West,  (Spades, Six))],
    [(South, (Spades, King)),   (West,  (Spades, Eight)),  (North, (Spades, Four)),    (East,  (Spades, Three))],
    [(South, (Spades, Seven)),  (West,  (Spades, Queen)),  (North, (Hearts, Queen)),   (East,  (Clubs, Five))],
    [(North, (Hearts, Four)),   (East,  (Hearts, Eight)),  (South, (Hearts, Nine)),    (West,  (Hearts, Two))],
    [(South, (Spades, Nine)),   (West,  (Spades, Jack)),   (North, (Hearts, Ace)),     (East,  (Clubs, Jack))],
    [(North, (Hearts, Seven)),  (East,  (Hearts, Six)),    (South, (Hearts, Ten)),     (West,  (Hearts, Three))],

    [(South, (Hearts, Jack)),   (West,  (Diamonds, Nine)), (North, (Diamonds, Two)),   (East,  (Hearts, Five))],

    [(South, (Spades, Ten)),    (West,  (Diamonds, Eight)), (North, (Diamonds, Three)),   (East,  (Diamonds, Queen))],
    [(South, (Hearts, King)),   (West,  (Diamonds, Jack)),  (North, (Diamonds, Five)),   (East,  (Diamonds, King))],

    -- E/W
    [(South, (Clubs, Seven)),   (West,  (Clubs, Nine)),    (North, (Diamonds, Seven)),  (East,  (Clubs, Queen))]
    
    ]


 -- Really need to have a checkPlay that takes a suit

-- checkSuitPlay :: Hands -> [Tricks] -> Bool
 

