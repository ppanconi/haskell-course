{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use foldr" #-}

-- Integer to list of digiy
toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0        = []
    | otherwise     = toDigits (n `div` 10) ++ [n `mod` 10]

revIntList :: [Integer] -> [Integer]
revIntList []       = []
revIntList (x:xs)   = revIntList xs ++ [x]

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []     = []
doubleEveryOtherRev [x] = [x]
doubleEveryOtherRev (x1:x2:xs) = doubleEveryOtherRev xs ++ [2*x2, x1]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = doubleEveryOtherRev (revIntList x)

sumListOfInt :: [Integer] -> Integer
sumListOfInt [] = 0
sumListOfInt (x:xs) = x + sumListOfInt xs

toDigitsList :: [Integer] -> [Integer]
toDigitsList [] = []
toDigitsList (x:xs) = toDigits x ++ toDigitsList xs


valididateCreditCardNumber :: Integer -> Bool
valididateCreditCardNumber x
    | x > 0 = sumListOfInt (toDigitsList (doubleEveryOther (toDigits x))) `mod` 10 == 0
    | otherwise = False

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 a b c = [(a, b)]
hanoi n a b c = hanoi (n - 1) a c b ++ hanoi 1 a b c ++ hanoi (n -1) c b a
