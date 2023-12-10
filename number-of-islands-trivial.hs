import Control.Monad.State
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Array
import Data.Foldable
import Control.Exception (assert)

type VisitedDistricts k = Set.Set k
type VisitedLands k = Set.Set k
type Trace k = [k]
type LandsVisitState l k = (l, VisitedDistricts k, VisitedLands k, Trace k)

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
        (lands, visited, visitedLands, trace) <- get
        let trace' = district : trace
            visited' = Set.insert district visited in do
            put (lands, visited', visitedLands, trace')
            n <- if isLand lands district && not (district `Set.member` visitedLands)
                then let (visitedLands', trace'') = exploreLand lands district visitedLands in do
                    put (lands, visited', visitedLands `Set.union` visitedLands', trace'' ++ trace')
                    return 1
                else do
                    return 0
            foldM exploreNeighbor n (neighboringDistricts lands district)
                    where exploreNeighbor n neighbor = do
                                                        (_, vs, _, _) <- get
                                                        if not (neighbor `Set.member` vs) then do
                                                            k <- searchLands neighbor
                                                            return (n + k)
                                                        else return n

    exploreLand :: l -> k -> VisitedDistricts k -> (VisitedDistricts k, Trace k)
    exploreLand lands district visited
        | not $ isLand lands district  = error "tring to explore a not land district"
        | otherwise = 
            let b = (Set.insert district visited, [district])
            in foldl' exploreNeighbor b $ neighboringDistricts lands district
            where exploreNeighbor (visited, trace) neighbor =
                    if isLand lands neighbor && not (neighbor `Set.member` visited) then
                        let (visited', trace') = exploreLand lands neighbor visited 
                        in (visited `Set.union` visited', trace' ++ trace)
                    else (visited, trace)

newtype MatrixLands = MatrixLands {matrix :: Array (Integer, Integer) Integer}

instance VisitableLands MatrixLands (Integer, Integer) where
  isLand lands k = matrix lands ! k == 1
  neighboringDistricts lands (i,j) =
    [ (i+1, j) | inRange (bounds (matrix lands)) (i+1, j) ] ++
    [ (i, j+1) | inRange (bounds (matrix lands)) (i, j+1) ] ++
    [ (i-1, j) | inRange (bounds (matrix lands)) (i-1, j) ] ++
    [ (i, j-1) | inRange (bounds (matrix lands)) (i, j-1) ]

countIslands :: MatrixLands -> IO Int
countIslands m = let (c, (_, _, _, trace)) = runState (searchLands $ head (indices (matrix m))) (m, Set.empty, Set.empty, [])
                 in do
                    print "-----------------------"
                    print $ "visited " ++ show (length trace) ++ " districts:"
                    let traceStr = foldr' (\t s -> s ++ ">" ++ show t) "" trace
                    print traceStr
                    return c

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
     1,0,0,1]
--2

main = do
    cmatrix0 <- countIslands matrix0
    print $ assert (cmatrix0 == 1) "ok matrix0"
    cmatrix23 <- countIslands matrix23
    print $ assert (cmatrix23 == 2) "ok matrix23"
    cmatrix33 <- countIslands matrix33
    print $ assert (cmatrix33 == 2) "ok matrix33"
    cmatrix44 <- countIslands matrix44
    print $ assert (cmatrix44 == 2) "ok matrix44"
    cmatrix44' <- countIslands matrix44'
    print $ assert (cmatrix44' == 3) "ok matrix44'"
    cmatrix44'' <- countIslands matrix44''
    print $ assert (cmatrix44'' == 2) "ok matrix44''"