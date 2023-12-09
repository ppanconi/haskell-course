{-# LANGUAGE BlockArguments #-}
import Control.Monad.State
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Array
import Data.Foldable
import Control.Exception (assert)

type VisitedDistricts k = Set.Set k
type LandsVisitState l k = (l, VisitedDistricts k, VisitedDistricts k)

class Ord k => VisitableLends l k where
    isLand :: l -> k -> Bool

    neighboringDistricts :: l -> k -> [k]

    searchLands :: k -> State (LandsVisitState l k) Int
    searchLands district = do
        (earth, visited, visitedLands) <- get
        n <- if isLand earth district && not (district `Set.member` visitedLands)
            then do
                put (earth, visited, visitedLands `Set.union` exploreLand earth district visitedLands)
                return 1
            else do
                put (earth, Set.insert district visited, visitedLands)
                return 0
        foldM exploreNeighbor n (neighboringDistricts earth district)
                where exploreNeighbor n neighbor = do
                                                    (_, visited, _) <- get
                                                    if not (neighbor `Set.member` visited) then do
                                                        k <- searchLands neighbor
                                                        return (n + k)
                                                    else return n
        
    exploreLand :: l -> k -> VisitedDistricts k -> VisitedDistricts k
    exploreLand earth district visited =
        foldl' exploreNeighbor (Set.insert district visited) $ neighboringDistricts earth district
            where exploreNeighbor visited neighbor =
                    if isLand earth neighbor && not (neighbor `Set.member` visited) then
                        visited `Set.union` exploreLand earth neighbor visited
                    else visited


newtype MatrixLends = MatrixLends {matrix :: Array (Integer, Integer) Integer}

instance VisitableLends MatrixLends (Integer, Integer) where
  isLand earth k = matrix earth ! k == 1
  neighboringDistricts earth (i,j) =
    [ (i+1, j) | inRange (bounds (matrix earth)) (i+1, j) ] ++
    [ (i, j+1) | inRange (bounds (matrix earth)) (i, j+1) ] ++
    [ (i-1, j) | inRange (bounds (matrix earth)) (i-1, j) ] ++
    [ (i, j-1) | inRange (bounds (matrix earth)) (i, j-1) ]

countIslands m = evalState (searchLands $ head (indices (matrix m))) (m, Set.empty, Set.empty)

matrix0 = MatrixLends $ listArray ((0,0),(0,0)) [1]
--1
matrix23 = MatrixLends
    $ listArray ((0,0),(1,2))
    [1,0,0,
     0,1,1]
--2
matrix33 = MatrixLends $ listArray ((0,0),(2,2)) [1,1,0, 0,1,1, 1,0,1]
--2
matrix44 = MatrixLends
    $ listArray ((0,0),(3,3))
    [0,0,0,1,
     0,0,0,0,
     0,0,1,0,
     0,0,0,0]
--2
matrix44' = MatrixLends
    $ listArray ((0,0),(3,3))
    [1,1,0,1,
     0,1,1,1,
     1,0,0,0,
     1,1,0,1]
--3
matrix44'' = MatrixLends
    $ listArray ((0,0),(3,3))
    [1,1,0,1,
     1,1,1,1,
     1,0,0,0,
     1,1,0,1]
--2

main = do
    print $ assert (countIslands matrix0 == 1) "ok matrix0"
    print $ assert (countIslands matrix23 == 2) "ok matrix23"
    print $ assert (countIslands matrix33 == 2) "ok matrix33"
    print $ assert (countIslands matrix44 == 2) "ok matrix44"
    print $ assert (countIslands matrix44' == 3) "ok matrix44'"
    print $ assert (countIslands matrix44'' == 2) "ok matrix44''"