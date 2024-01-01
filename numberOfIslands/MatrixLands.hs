
import Data.Array
import VisitableLands
import SetVisitLogbook
import Control.Monad.State
import qualified Data.Set as S
import Data.Foldable
import Control.Exception (assert)

newtype MatrixLands = MatrixLands {matrix :: Array (Integer, Integer) Integer}

matrixIsLand :: MatrixLands -> (Integer, Integer) -> Bool
matrixIsLand l k = matrix l ! k == 1
matrixNeighboringDistricts :: MatrixLands -> (Integer, Integer) -> [(Integer, Integer)]
matrixNeighboringDistricts l (i,j) =
    [ (i+1, j) | inRange (bounds (matrix l)) (i+1, j) ] ++
    [ (i, j+1) | inRange (bounds (matrix l)) (i, j+1) ] ++
    [ (i-1, j) | inRange (bounds (matrix l)) (i-1, j) ] ++
    [ (i, j-1) | inRange (bounds (matrix l)) (i, j-1) ]

instance VisitableLands MatrixLands (Integer, Integer) where
  neighboringDistricts :: MatrixLands -> (Integer, Integer) -> [(Integer, Integer)]
  neighboringDistricts l (i,j) = matrixNeighboringDistricts l (i,j)
  isLand = matrixIsLand

searchMatrix :: MatrixLands -> State (SetVisitLogbook (Integer, Integer)) Int
searchMatrix m = searchLands m $ head (indices (matrix m))

initialLogbook :: SetVisitLogbook (Integer, Integer)
initialLogbook = SetVisitLogbook S.empty S.empty []

countIslandsWithTrace :: MatrixLands -> IO Int
countIslandsWithTrace m =
    let (c, SetVisitLogbook{trace}) =
            runState (searchMatrix m) initialLogbook
        in do
            print "-----------------------"
            print $ "counted " ++ show c ++ " islands"
            print $ "visited " ++ show (length trace) ++ " districts:"
            let traceStr = foldr' (\t s -> s ++ ">" ++ show t) "" trace
            print traceStr
            return c

matrix0 = MatrixLands $ listArray ((0,0),(0,0)) [1]
--1
matrix22 = MatrixLands
    $ listArray ((0,0),(1,1))
    [0,1,
     1,0]
--2
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
    c <- countIslandsWithTrace matrix0
    print $ assert (c == 1) "ok matrix0"
    c <- countIslandsWithTrace matrix22
    print $ assert (c == 2) "ok matrix22"
    c <- countIslandsWithTrace matrix23
    print $ assert (c == 2) "ok matrix23"
    c <- countIslandsWithTrace matrix33
    print $ assert (c == 2) "ok matrix33"
    c <- countIslandsWithTrace matrix44
    print $ assert (c == 2) "ok matrix44"
    c <- countIslandsWithTrace matrix44'
    print $ assert (c == 3) "ok matrix44'"
    c <- countIslandsWithTrace matrix44''
    print $ assert (c == 2) "ok matrix44''"
    c <- countIslandsWithTrace matrix1010
    print $ assert (c == 9) "ok cmatrix1010"