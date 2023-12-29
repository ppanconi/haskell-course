
import Data.Array
import qualified VisitableLands as VL
import SetVisitLogbook
import Control.Monad.State
import qualified Data.Set as S
import Data.Foldable
import Control.Exception (assert)

newtype MatrixLands = MatrixLands {matrix :: Array (Integer, Integer) Integer}

isLand :: MatrixLands -> (Integer, Integer) -> Bool
isLand l k = matrix l ! k == 1
neighboringDistricts :: MatrixLands -> (Integer, Integer) -> [(Integer, Integer)]
neighboringDistricts l (i,j) =
    [ (i+1, j) | inRange (bounds (matrix l)) (i+1, j) ] ++
    [ (i, j+1) | inRange (bounds (matrix l)) (i, j+1) ] ++
    [ (i-1, j) | inRange (bounds (matrix l)) (i-1, j) ] ++
    [ (i, j-1) | inRange (bounds (matrix l)) (i, j-1) ]

instance VL.VisitableLands MatrixLands (Integer, Integer) where
  neighboringDistricts l (i,j) = neighboringDistricts l (i,j)
  isLand = isLand

countIslandsWithTrace :: MatrixLands -> IO Int
countIslandsWithTrace m = let (c, SetVisitLogbook{trace}) = runState (VL.searchLands m $ head (indices (matrix m))) $ SetVisitLogbook S.empty S.empty S.empty []
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