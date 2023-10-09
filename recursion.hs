maximun' :: (Ord a) => [a] -> a
maximun' [] = error "maximun of empty list"
maximun' [x] = x
maximun' (x:xs)
    | x > m = x
    | otherwise = m
    where m = maximun' xs

maximun'' :: (Ord a) => [a] -> a
maximun'' [] = error "maximun of empty list"
maximun'' [x] = x
maximun'' (x:xs) = max x (maximun'' xs)

replicate' ::  (Ord i, Num i) => i-> a -> [a]
replicate' i a
    | i <= 0 = []
    | otherwise = a : replicate' (i - 1) a

take' :: (Ord i, Num i) => i -> [a] -> [a]
take' i xs
    | i <= 0 = []
take' _ [] = []
take' i (x:xs) = x : take' (i-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' a = a : repeat' a

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs)
    | a == x = True
    | otherwise = elem' a xs

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort largers ++ [x] ++  quicksort  minors
    where
        largers = filter (x <)  xs
        minors = filter (x >=)  xs

calcBmis :: (RealFloat a) => [(a, a)] -> [a]  
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2
-- , bmi >= 25.0
                ]  