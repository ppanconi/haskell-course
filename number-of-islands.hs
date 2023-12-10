import Control.Monad.State
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Array
import Data.Foldable
import Control.Exception (assert)

type VisitedDistricts k = Set.Set k
type VisitedLands k = Set.Set k
type LandsVisitState l k = (l, VisitedDistricts k, VisitedLands k)

-- data LandsVisitState l k = LandsVisitState {
--     lands :: l
--     , visited :: VisitedDistricts k
--     , visitedLands :: VisitedLands k
--     , trace :: [k]

-- }

class Ord k => VisitableLands l k where
    isLand :: l -> k -> Bool

    neighboringDistricts :: l -> k -> [k]

    searchLands :: k -> State (LandsVisitState l k) Int
    searchLands district = do
        (lands, visited, visitedLands) <- get
        n <- if isLand lands district && not (district `Set.member` visitedLands)
            then do
                put (lands, visited, visitedLands `Set.union` exploreLand lands district visitedLands)
                return 1
            else do
                put (lands, Set.insert district visited, visitedLands)
                return 0
        foldM exploreNeighbor n (neighboringDistricts lands district)
                where exploreNeighbor n neighbor = do
                                                    (_, visited, _) <- get
                                                    if not (neighbor `Set.member` visited) then do
                                                        k <- searchLands neighbor
                                                        return (n + k)
                                                    else return n
        
    exploreLand :: l -> k -> VisitedDistricts k -> VisitedDistricts k
    exploreLand lands district visited =
        foldl' exploreNeighbor (Set.insert district visited) $ neighboringDistricts lands district
            where exploreNeighbor visited neighbor =
                    if isLand lands neighbor && not (neighbor `Set.member` visited) then
                        visited `Set.union` exploreLand lands neighbor visited
                    else visited


newtype MatrixLands = MatrixLands {matrix :: Array (Integer, Integer) Integer}

instance VisitableLands MatrixLands (Integer, Integer) where
  isLand lands k = matrix lands ! k == 1
  neighboringDistricts lands (i,j) =
    [ (i+1, j) | inRange (bounds (matrix lands)) (i+1, j) ] ++
    [ (i, j+1) | inRange (bounds (matrix lands)) (i, j+1) ] ++
    [ (i-1, j) | inRange (bounds (matrix lands)) (i-1, j) ] ++
    [ (i, j-1) | inRange (bounds (matrix lands)) (i, j-1) ]

countIslands m = evalState (searchLands $ head (indices (matrix m))) (m, Set.empty, Set.empty)

matrix0 = MatrixLands $ listArray ((0,0),(0,0)) [1]
--1
matrix23 = MatrixLands
    $ listArray ((0,0),(1,2))
    [1,0,0,
     0,1,1]
--2
matrix33 = MatrixLands $ listArray ((0,0),(2,2)) [1,1,0, 0,1,1, 1,0,1]
--2
matrix44 = MatrixLands
    $ listArray ((0,0),(3,3))
    [0,0,0,1,
     0,0,0,0,
     0,0,1,0,
     0,0,0,0]
--2
matrix44' = MatrixLands
    $ listArray ((0,0),(3,3))
    [1,1,0,1,
     0,1,1,1,
     1,0,0,0,
     1,1,0,1]
--3
matrix44'' = MatrixLands
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