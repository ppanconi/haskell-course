import Data.List (foldl', scanl')
-- Ex 1
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x-2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Ex 2

-- utility function to split a list
splitlist list = splitAt ((length list + 1) `div` 2) list

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show)

-- 2 balanced Tree are equals if they have the same height
instance Eq (Tree a) where
    Leaf == Leaf = True
    Node x _ _ _ == Leaf = False
    Node x _ _ _ == Node y _ _ _ = y == x

-- convention height Leaf = -1
heightTree Leaf = -1
heightTree (Node n _ _ _)= n

-- foldTree 1st implemetation 
-- merging two balanced Trees 
foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree (x:xs) =
    let (ys,zs) = splitlist xs
    in mergeTrees x (foldTree ys) (foldTree zs)

mergeTrees :: a -> Tree a -> Tree a -> Tree a
mergeTrees x tl tr =
    let h = 1 + max (heightTree tl) (heightTree  tr)
    in Node h tl x tr

foldTree' :: [a] -> Tree a
foldTree' = foldr insert Leaf

treeWithHeight :: Integer -> Tree Integer
treeWithHeight h = head $ dropWhile (\t -> heightTree t < h) $ scanl (\t n -> insert n t) Leaf [1..]

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h Leaf y Leaf) = Node 1 (insert x Leaf) y Leaf
insert x (Node h Leaf y right) = Node h (insert x Leaf) y right
insert x (Node h left y Leaf) = Node h left y (insert x Leaf)
insert x (Node h left y right) =
    let (leftH, rightH) = (heightTree left, heightTree right) in
    case compare leftH rightH of
        LT -> Node h (insert x left) y right
        GT -> Node h left y (insert x right)
        EQ -> Node (1 + heightTree right') left y right'
                where right' = insert x right