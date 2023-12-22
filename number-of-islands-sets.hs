import Control.Monad.State
import qualified Data.Set as S
import Data.List (foldl')
import Data.Array
import Data.Foldable
import Control.Exception (assert)
data LandsVisitState l k = LandsVisitState {
      visited :: S.Set k
    , landsToVisit :: S.Set k
    , notLandsToVisit :: S.Set k
    , trace :: [k]
}

next :: LandsVisitState l k -> Maybe (k, LandsVisitState l k)
next (LandsVisitState v landsToVisit notLandsToVisit t)
        | not (null landsToVisit) = let (k, landsToVisit') = S.deleteFindMin landsToVisit in Just (k, LandsVisitState v landsToVisit' notLandsToVisit t )
        | not (null notLandsToVisit) = let (k, notLandsToVisit') = S.deleteFindMin notLandsToVisit in Just (k, LandsVisitState v landsToVisit notLandsToVisit' t )
        | null landsToVisit && null notLandsToVisit = Nothing

class Ord k => VisitableLands l k where
    
    isLand :: l -> k -> Bool

    neighboringDistricts :: l -> k -> [k]

    searchLands :: l -> k -> State (LandsVisitState l k) Int
    searchLands l k = do
        LandsVisitState visited landsToVisit notLandsToVisit trace <- get

        let trace' = k : trace
            visited' = S.insert k visited
            neighbors = neighboringDistricts l k
            -- if the starting element in visit is a land we need to count it
            isStartingDistrictALand = if isLand l k && null visited then 1 else 0
            -- we evaluate current element neighbors avoiding already know elements.
            -- 
            (landsToVisit', notLandsToVisit', n') = foldr evaluateNeighbor (landsToVisit, notLandsToVisit, isStartingDistrictALand)
                    $ filter unknownNeighbors neighbors
                where 
                    unknownNeighbors k' = S.notMember k' visited && S.notMember k' landsToVisit && S.notMember k' notLandsToVisit
                    evaluateNeighbor k' (ls, nls, n)
                        | isLand l k' && null ls && not (isLand l k) = (S.insert k' ls, nls, 1)
                        | isLand l k' = (S.insert k' ls, nls, n)
                        | otherwise = (ls, S.insert k' nls, n)
            nexts = next $ LandsVisitState visited' landsToVisit' notLandsToVisit' trace'
            -- we select the next district to visit with
            -- priority on lands in the stack          
            in case nexts of
                    Nothing -> do
                            put (LandsVisitState visited' landsToVisit' notLandsToVisit' trace')
                            return n'
                    Just (k'', s'') -> do
                        put s''
                        n'' <- searchLands l k''
                        return (n' + n'')
                    

newtype MatrixLands = MatrixLands {matrix :: Array (Integer, Integer) Integer}

instance VisitableLands MatrixLands (Integer, Integer) where
  isLand l k = matrix l ! k == 1
  neighboringDistricts l (i,j) =
    [ (i+1, j) | inRange (bounds (matrix l)) (i+1, j) ] ++
    [ (i, j+1) | inRange (bounds (matrix l)) (i, j+1) ] ++
    [ (i-1, j) | inRange (bounds (matrix l)) (i-1, j) ] ++
    [ (i, j-1) | inRange (bounds (matrix l)) (i, j-1) ]

countIslandsWithTrace :: MatrixLands -> IO Int
countIslandsWithTrace m = let (c, LandsVisitState{trace}) = runState (searchLands m $ head (indices (matrix m))) $ LandsVisitState S.empty S.empty S.empty []
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

matrix1010 = MatrixLands
    $ listArray ((0,0),(9,9))
    [1,1,0,1,1,0,0,0,1,0,
     1,1,1,1,1,1,0,0,0,1,
     1,0,0,0,1,1,1,1,0,1,
     1,0,0,1,0,0,0,1,1,0,
     1,0,0,1,0,0,0,1,0,0,
     0,0,1,1,0,0,0,0,1,1,
     0,0,0,1,0,1,0,0,0,1,
     0,0,1,0,0,1,1,1,1,0,
     1,0,0,1,0,0,0,1,0,0,
     1,1,0,1,0,0,0,1,0,0]
-- 9

main = do
    cmatrix0 <- countIslandsWithTrace matrix0
    print $ assert (cmatrix0 == 1) "ok matrix0"
    cmatrix23 <- countIslandsWithTrace matrix23
    print $ assert (cmatrix23 == 2) "ok matrix23"
    cmatrix33 <- countIslandsWithTrace matrix33
    print $ assert (cmatrix33 == 2) "ok matrix33"
    cmatrix44 <- countIslandsWithTrace matrix44
    print $ assert (cmatrix44 == 2) "ok matrix44"
    cmatrix44' <- countIslandsWithTrace matrix44'
    print $ assert (cmatrix44' == 3) "ok matrix44'"
    cmatrix44'' <- countIslandsWithTrace matrix44''
    print $ assert (cmatrix44'' == 2) "ok matrix44''"
    cmatrix1010 <- countIslandsWithTrace matrix1010
    print $ assert (cmatrix1010 == 9) "ok cmatrix1010"