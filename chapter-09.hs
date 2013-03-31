-- Exercise 9.1

fact :: Integer -> Integer
fact n
    | n == 0    = 1
    | otherwise = n * fact (n - 1)

{-
    ghci> (4 > 2) || (fact (-1) == 17)
    True -- Because of laziness, fact never gets called.

    ghci> (4 > 2) && (fact (-1) == 17)
    -- hangs forever because fact (-1) isn't handled well.
-}

-- Exercise 9.2

mult :: Integer -> Integer -> Integer
mult x y
    | x == 0    = 0
    | otherwise = x * y

{-
    ghci> mult 0 (fact (-2))
    0     -- again, being lazy, the call to fact is never done
-}


-- Exercise 9.3

{-
    Proof that

A   flipV (flipH pic) = flipH (flipV pic)
B   flipV (flipV pic) = pic
C   flipH (flipH pic) = pic

Proof of A
==========





-}



-- Exercise 9.4



-- Exercise 9.5



-- Exercise 9.6



-- Exercise 9.7



-- Exercise 9.8



-- Exercise 9.9



-- Exercise 9.10



-- Exercise 9.11



-- Exercise 9.12



-- Exercise 9.13



-- Exercise 9.14



-- Exercise 9.15



