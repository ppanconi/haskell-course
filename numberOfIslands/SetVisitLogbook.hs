module SetVisitLogbook where

    import qualified Data.Set as S
    import Data.Array
    import Data.Foldable
    import qualified VisitableLands

    data SetVisitLogbook k = SetVisitLogbook {
        visited :: S.Set k
        , stackedToVisit :: S.Set k
        , trace :: [k]
    }

    instance Ord k => VisitableLands.Logbook (SetVisitLogbook k) k where
        isNotVisited :: Ord k => SetVisitLogbook k -> k -> Bool
        isNotVisited l k = S.notMember k (visited l)
        registerVisited :: Ord k => SetVisitLogbook k -> k -> SetVisitLogbook k
        registerVisited l k = l { visited = S.insert k $ visited l, stackedToVisit = S.delete k $ stackedToVisit l, trace = k:trace l }
        stackedToVisit :: Ord k => SetVisitLogbook k -> [k]
        stackedToVisit l = S.elems $ stackedToVisit l
        stackToVisit :: Ord k => SetVisitLogbook k -> k -> SetVisitLogbook k
        stackToVisit l k = l { stackedToVisit = S.insert k $ stackedToVisit l}