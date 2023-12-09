{-# LANGUAGE BlockArguments #-}
import Control.Monad.State
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Array
import Data.Foldable

type VisitedDistricts k = Set.Set k
type EarthVisitState earth k = (earth, VisitedDistricts k, VisitedDistricts k, Int)

class Ord k => VisitableEarth earth k where
    isLand :: earth -> k -> Bool

    neighboringDistricts :: earth -> k -> [k]

    searchLands :: k -> State (EarthVisitState earth k) Int
    searchLands district = do
        (earth, visited, visitedLands, n) <- get
        if isLand earth district && not (district `Set.member` visitedLands) then
            put (earth, visited, visitedLands `Set.union` exploreLand earth district visitedLands, n + 1 )
        else
            put (earth, Set.insert district visited, visitedLands, n)
        for_ (neighboringDistricts earth district) $ \neighbor -> do
              (_, visited, _, _) <- get
              unless (neighbor `Set.member` visited) do
                searchLands neighbor
                return ()
        (_, _, _, n) <- get
        return n

    exploreLand :: earth -> k -> VisitedDistricts k -> VisitedDistricts k
    exploreLand earth district visited =
        foldl' exploreNeighbor (Set.insert district visited) $ neighboringDistricts earth district
            where exploreNeighbor visited neighbor =
                    if isLand earth neighbor && not (neighbor `Set.member` visited) then
                        visited `Set.union` exploreLand earth neighbor visited
                    else visited


newtype MatrixVisitableEarth = MatrixVisitableEarth {matrix :: Array (Integer, Integer) Integer}

instance VisitableEarth MatrixVisitableEarth (Integer, Integer) where
  isLand earth k = matrix earth ! k == 1
  neighboringDistricts earth (i,j) =
    [ (i+1, j) | inRange (bounds (matrix earth)) (i+1, j) ] ++
    [ (i, j+1) | inRange (bounds (matrix earth)) (i, j+1) ] ++
    [ (i-1, j) | inRange (bounds (matrix earth)) (i-1, j) ] ++
    [ (i, j-1) | inRange (bounds (matrix earth)) (i, j-1) ]

countIslands m = evalState (searchLands $ head (indices (matrix m))) (m, Set.empty, Set.empty, 0)


matrix0 = MatrixVisitableEarth $ listArray ((0,0),(0,0)) [1]
--1
matrix23 = MatrixVisitableEarth $ listArray ((0,0),(1,2)) [1,0,0, 0,1,1]
--2
matrix33 = MatrixVisitableEarth $ listArray ((0,0),(2,2)) [1,1,0, 0,1,1, 1,0,1]
--2
matrix44 = MatrixVisitableEarth $ listArray ((0,0),(3,3)) [0,0,0,1, 0,0,0,0, 0,0,1,0, 0,0,0,0]
--2
matrix44' = MatrixVisitableEarth $ listArray ((0,0),(3,3)) [1,1,0,1, 0,1,1,1, 1,0,0,0, 1,1,0,1]
--3
matrix44'' = MatrixVisitableEarth $ listArray ((0,0),(3,3)) [1,1,0,1, 1,1,1,1, 1,0,0,0, 1,1,0,1]
--2