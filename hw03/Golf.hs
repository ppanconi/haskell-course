module Golf where

-- Exercise 1
skips :: [a] -> [[a]]
skips xs = [ [ x | (x, i) <- zip xs [1..], i `mod` m == 0 ] | m <- [1..length xs] ]

-- Exercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:xs) = snd $ foldl (\((y,z), ms) x -> if z>y && z>x then ((z,x), ms ++ [z]) else ((z,x), ms)) ((x1, x2), []) xs
localMaxima _ = []

-- Exercise 3 Histogram
digitDistr :: [Integer] -> [Integer]
digitDistr = foldl  updateDigitDistr (replicate 10 0)
    where updateDigitDistr ds n = hs ++ [v + 1] ++ ts
            where (hs, v:ts) = splitAt (fromIntegral n) ds

levelStr :: [Integer] -> Integer -> String
levelStr xs l = map (\x -> if x < l then ' ' else '*') xs

histogram :: [Integer] -> String
histogram xs = unlines $ map (levelStr distr) [maxFr, maxFr-1..1] ++ ["=========="] ++ ["0123456789"]
    where distr = digitDistr xs
          maxFr = maximum distr