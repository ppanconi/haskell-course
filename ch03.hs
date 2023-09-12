type CustomerID = Int
type Address = [String]

data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)
customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

data List a = Cons a (List a)
            | Nil
              deriving (Show)

fromList (x:xs) = Cons x (fromList xs)
fromList []     = Nil

fromMyList :: List a -> [a]
fromMyList (Cons x xs) = x : fromMyList xs
fromMyList Nil = []

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving Show

myTree :: Tree String
myTree = Node "parent" (Node "left child" Empty Empty)
                           (Node "right child" Empty Empty) 
                      
data UglyTree a = UglyTree (Maybe a) (Maybe (UglyTree a)) (Maybe (UglyTree a))