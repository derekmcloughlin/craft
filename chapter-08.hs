import Data.List

-- Rock Paper Scissors from Chapter 4

data Move = Rock |
            Paper |
            Scissors
            deriving (Ord, Show, Eq)

beat :: Move -> Move
beat Rock = Paper
beat Paper = Scissors
beat _ = Rock

lose :: Move -> Move
lose Rock = Scissors
lose Paper = Rock
lose _ = Paper

data Result = Win |
              Lose |
              Draw
              deriving (Show, Eq)

opposite :: Result -> Result
opposite Win = Lose
opposite Lose = Win
opposite Draw = Draw

type Tournament = ([Move], [Move])

exampleTournament = ([Rock, Rock, Paper], [Scissors, Paper, Rock])

-- Exercise 8.1

-- Similar to the 'outcome' function in 4.12

outcome :: Move -> Move -> Integer

outcome Rock Scissors     = 1
outcome Paper Rock        = 1
outcome Paper Scissors    = 1
outcome Scissors Rock     = -1
outcome Rock Paper        = -1
outcome Scissors Paper    = -1
outcome Rock Rock         = 0
outcome Paper Paper       = 0
outcome Scissors Scissors = 0

-- Exercise 8.2

tournamentOutcome :: Tournament -> Integer

tournamentOutcome ([], []) = 0
tournamentOutcome (as, bs) = sum [outcome a b | (a, b) <- zip as bs]

{- Alternatively:
    tournamentOutcome ((a:as), (b:bs)) = outcome a b + tournamentOutcome (as, bs)
-}

-- Exercise 8.3

type Strategy = [Move] -> Move

beatLastMove :: Strategy
beatLastMove [] = Rock
beatLastMove (latest:rest) = beat latest

loseLastMove :: Strategy
loseLastMove [] = Scissors
loseLastMove (latest:rest) = lose latest

-- Exercise 8.4

lastTwo :: Strategy

{- This works because e.g. if they played Rock twice, and assuming they
   won't play Rock again, they'll play Scissors or Paper. Whatever loses to
   Rock will either beat this or draw.
-}

lastTwo moves@(prev1:prev2:rest)
    | prev1 == prev2   = lose prev1
    | otherwise        = beatLastMove moves -- or anything else

lastTwo (latest:rest) = lose latest
lastTwo [] = Rock


-- Exercise 8.5

frequencyStrategy :: Strategy

frequencyStrategy moves = beat $ fst $ minimum [(Rock, count Rock),
                                   (Scissors, count Scissors),
                                   (Paper, count Paper)]
                          where 
                          count a = sum [1 | move <- moves, move == a]


-- Exercise 8.6

-- Harder

-- Exercise 8.7

-- Harder

-- Exercise 8.8

-- Hard

-- Exercise 8.9

-- Hard

-- Exercise 8.10

testPalindrome :: IO ()
testPalindrome =
    do
        putStrLn "Enter your name:"
        line <- getLine
        case isPalindrome line of
             True  -> putStrLn $ "This is a palindrome:" ++ line
             False -> putStrLn $ "This is NOT a palindrome:" ++ line

    where
    isPalindrome s = True -- TODO
             
-- Exercise 8.11

getInt =
    do
        line <- getLine
        return (read line :: Integer)

addTwoNumbers :: IO ()
addTwoNumbers =
    do
        putStr "Enter the 1st number: "
        firstNum <- getInt
        putStr "Enter the 2nd number: "
        secondNum <- getInt
        let result = firstNum + secondNum
        putStr "The result of "
        print firstNum
        putStr " + "
        print secondNum
        putStr " is "
        print result
        

-- Exercise 8.12

putNtimes :: Integer -> String -> IO ()
putNtimes n s =
    do
        let t = concat $ take (fromIntegral n) $ repeat $ s ++ "\n"
        putStr t


-- Exercise 8.13

addNnumbers :: IO ()
addNnumbers =
    do
        putStr "Enter the number of integers: "
        ntimes <- getInt
        thesum <- add_the_numbers ntimes
        print thesum
    where 
    add_the_numbers n =
        do
            if n <= 0 then
               return 0
            else do
                putStr "Enter the Integer: "
                summand <- getInt
                rest <- add_the_numbers (n - 1)
                return (summand + rest)

-- Exercise 8.14


wc :: IO ()
wc = do
        line <- getLine
        if line == "" then
           return ()
        else do
            putStrLn line
            wc
            

-- Exercise 8.15

-- Need palindrome implementation

-- Exercise 8.16

-- More palindrome stuff.

-- Exercise 8.17


addNumbersRepeat :: IO ()
addNumbersRepeat =
    do
        putStrLn "Enter the integers one by one until. To stop enter 0."
        thesum <- add_the_numbers
        print thesum
    where 
    add_the_numbers =
        do
            n <- getInt
            if n == 0 then
               return 0
            else do
                rest <- add_the_numbers
                return (n + rest)


-- Exercise 8.18

addNumbersSort :: IO ()
addNumbersSort =
    do
        putStrLn "Enter the integers one by one until. To stop enter 0."
        numbers <- add_the_numbers
        putStrLn "The sorted numbers are:"
        -- Cheating - he probably wants to do the sort inline somehow as the numbers come in.
        print $ sort numbers
    where 
    add_the_numbers =
        do
            n <- getInt
            if n == 0 then
               return []
            else do
                rest <- add_the_numbers
                return (n : rest)


-- Exercise 8.19

copy :: IO ()
copy =
    do
        line <- getLine
        let whileCopy = do
                if (line == "")
                then return ()
                else do
                    putStrLn line
                    line <- getLine
                    whileCopy
        whileCopy
               
{-  The function never terminates unless you enter a newline at the very start.
    This is because when whileCopy gets called again recursively, "line" is re-set
    to the value it had outside the definition.
-}

-- Exercise 8.20

step :: Strategy -> Strategy -> Tournament -> Tournament
step strategyA strategyB (movesA, movesB) =
     (strategyA movesB : movesA,  -- What A will play
      strategyB movesA : movesB)  -- What B will Play



playSvsS :: Strategy -> Strategy -> Integer -> Tournament

playSvsS strategyA strategyB n =
    turn ([], []) n
    where
    turn t m =
        if (m <= 0) then
            t
        else
            turn (strategyA (snd t) : snd t, strategyB (fst t) : fst t) (m - 1)
          

testfn :: Integer -> Integer -> Integer -> Integer
testfn a b c =
       if a <= 0 then
          b
       else
          c
          
      
-- Exercise 8.21

type Strategy2 = Tournament -> Move




-- Exercise 8.22



-- Exercise 8.23



