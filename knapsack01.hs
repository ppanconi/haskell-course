import Data.Array

knapsack :: [Double] -- values
            -> [Integer] -- non negative weights 
            -> Integer  -- max knapsack capacity
            -> Double -- return max value
knapsack vs ws maxW = matrix!(numOfItems - 1, maxW)
    where
        numOfItems = length vs
        matrix = array ((-1, 0), (numOfItems - 1, maxW)) $
            [((-1, w), 0) | w <- [0..maxW]] ++
            [((i, 0), 0) | i <- [0..numOfItems - 1]] ++
            [((i, w), best) | i <- [0..numOfItems - 1]
                            , w <- [1.. maxW] 
                            , let best 
                                    | ws!!i > w = matrix!(i-1, w)
                                    | otherwise = max (matrix!(i-1,w)) (matrix!(i-1, w - ws!!i) + vs!!i)
                            ]
