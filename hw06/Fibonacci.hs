{-# OPTIONS_GHC -fno-warn-missing-methods #-}

import Data.Array
import Data.List

-- Exercise 1
fib n
    | n==0 = 0
    | n==1 = 1
    | otherwise = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]

-- Exercise 2
fibs2 :: [Integer]
fibs2 = fib2 0 1 where
    fib2 a b = a : fib2 b (a + b)

fibsA    :: Int -> Array Int Int
fibsA n  =  a  where a = array (0,n) ([(0, 0), (1, 1)] ++
                                     [(i, a!(i-2) + a!(i-1)) | i <- [2..n]])

-- Exercise 3 
data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x : streamToList s

instance Show a => Show (Stream a) where
    -- show s = intercalate "," (map show (take 20 $ streamToList s) ++ ["..."] )
    show s = intercalate "," (map show (streamToList s) )

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x s) = Cons (f x) $ streamMap f s

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a $ streamFromSeed f (f a)

-- Exercise 5
-- natural numbers as Stream Integer
nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons y ys) zs = Cons y (interleaveStreams zs ys)

-- [0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,
--  0,3,0,1,0,2,0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,
--  0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,6,0,1,
--  0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,
--  0,1,0,2,0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,
--  0,4,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,7,0,1,0,2,
--  0,1,0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,
--  0,2,0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,
--  0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,6,0,1,0,2,0,1,
--  0,3,0,1,0,2,0,1,0,4,0,1,0,2,0,1,0,3,0,1,0,2,
--  0,1,0,5,0,1,0,2,0,1,0,3,0,1,0,2,0,1,0,4,0,1,
--  0,2,0,1,0,3,0,1,0,2,0...
ruler :: Stream Integer
ruler = interRuler 0
    where interRuler y = interleaveStreams (streamRepeat y) (interRuler (y+1))

-- Exercise 6
-- x = 0 + 1x + 0x2 + 0x3 + . . .
-- 0 1 0 0 0 0 0
x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  (+) (Cons x xs) (Cons y ys)  = Cons (x+y) (xs + ys)
--   (-) = _
  (*) (Cons x xs) (Cons y ys) = Cons (x*y) ( streamMap (*x) ys + (xs * Cons y ys))
  negate = streamMap negate
--   abs = _
--   signum = _
  fromInteger x = Cons x (streamRepeat 0)

instance Fractional (Stream Integer) where
    (/) (Cons y ys) (Cons x xs) = q
        where q = Cons (y `div` x) (streamMap (`div` x) (ys - q * xs))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

-- Exercise 7
data Matrix a = Matrix ((a, a), (a, a))

instance Num (Matrix Integer) where
    (Matrix ((a, b), (c, d))) * (Matrix ((e, f), (g, h))) = Matrix
        ((a * e + b * g, a * f + b * h),
        (c * e + d * g, c * f + d * h))

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 1 = 1
fib4 n = f ((Matrix ((1, 1), (1, 0)))^(n-1)) where f (Matrix ((a,_), _)) = a