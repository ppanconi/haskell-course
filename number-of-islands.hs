import Control.Monad.State
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Array
import Data.Foldable
import Control.Exception (assert)

data LandsVisitState l k = LandsVisitState {
    lands :: l
    , visited :: Set.Set k
    , landsToVisit :: [k]
    , notLandsToVist :: [k]
    , trace :: [k]
}

class Ord k => VisitableLands l k where
    isLand :: l -> k -> Bool

    neighboringDistricts :: l -> k -> [k]

    searchLands :: k -> State (LandsVisitState l k) Int
    searchLands k = do
        LandsVisitState lands visited landsToVisit notLandsToVist trace <- get
        let trace' = k : trace
            visited' = Set.insert k visited
            (landsToVisit', notLandsToVist', n') =
                foldr evaluateNeighbor (landsToVisit, notLandsToVist, if isLand lands k && null visited then 1 else 0) $ filter newNeighbor (neighboringDistricts lands k)
                where evaluateNeighbor k' (ls, nls, n)
                        | isLand lands k' && null ls && not (isLand lands k) = (k' : ls, nls, 1)
                        | isLand lands k' = (k' : ls, nls, n)
                        | otherwise = (ls, k' : nls, n)
                      newNeighbor k' = Set.notMember k' visited && notElem k' landsToVisit && notElem k' notLandsToVist
            in do
                case landsToVisit' of
                    (k'':landsToVisit'') -> do
                        put (LandsVisitState lands visited' landsToVisit'' notLandsToVist' trace')
                        n'' <- searchLands k''
                        return (n' + n'')    
                    [] -> case notLandsToVist' of
                        (k'': notLandsToVist'') -> do
                            put (LandsVisitState lands visited' landsToVisit' notLandsToVist'' trace')
                            n'' <- searchLands k''
                            return (n' + n'')
                        [] -> do 
                            put (LandsVisitState lands visited' landsToVisit' notLandsToVist' trace')
                            return n'    

newtype MatrixLands = MatrixLands {matrix :: Array (Integer, Integer) Integer}

instance VisitableLands MatrixLands (Integer, Integer) where
  isLand lands k = matrix lands ! k == 1
  neighboringDistricts lands (i,j) =
    [ (i+1, j) | inRange (bounds (matrix lands)) (i+1, j) ] ++
    [ (i, j+1) | inRange (bounds (matrix lands)) (i, j+1) ] ++
    [ (i-1, j) | inRange (bounds (matrix lands)) (i-1, j) ] ++
    [ (i, j-1) | inRange (bounds (matrix lands)) (i, j-1) ]

countIslands :: MatrixLands -> IO Int
countIslands m = let (c, LandsVisitState{trace}) = runState (searchLands $ head (indices (matrix m))) $ LandsVisitState m Set.empty [] [] []
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
matrix33 = MatrixLands $ listArray ((0,0),(2,2)) 
    [1,1,0, 
     0,1,1, 
     1,0,1]
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
    print cmatrix23
    print $ assert (cmatrix23 == 2) "ok matrix23"
    cmatrix33 <- countIslands matrix33
    print $ assert (cmatrix33 == 2) "ok matrix33"
    cmatrix44 <- countIslands matrix44
    print $ assert (cmatrix44 == 2) "ok matrix44"
    cmatrix44' <- countIslands matrix44'
    print $ assert (cmatrix44' == 3) "ok matrix44'"
    cmatrix44'' <- countIslands matrix44''
    print $ assert (cmatrix44'' == 2) "ok matrix44''"