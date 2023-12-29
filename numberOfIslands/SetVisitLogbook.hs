module SetVisitLogbook where

    import qualified Data.Set as S
    import Data.Array
    import Data.Foldable
    import qualified VisitableLands

    data SetVisitLogbook k = SetVisitLogbook {
        visited :: S.Set k
        , landsToVisit :: S.Set k
        , notLandsToVisit :: S.Set k
        , trace :: [k]
    }

    instance Ord k => VisitableLands.Logbook (SetVisitLogbook k) k where
        justBegun a = null $ visited a
        isUnknownDistrict a k =  S.notMember k (visited a) && S.notMember k (landsToVisit a) && S.notMember k (notLandsToVisit a)
        isThereALandToVisit a = not $ null $ visited a
        addALandToVisit a k = a { landsToVisit = S.insert k $ landsToVisit a }
        addANotLandToVisit a k = a { notLandsToVisit = S.insert k $ notLandsToVisit a }
        next a k
            | not $ null $ landsToVisit a =
                let (k', landsToVisit') = S.deleteFindMin $ landsToVisit a
                in Just (k', a { notLandsToVisit = landsToVisit', visited = S.insert k $ visited a})
            | not $ null $ notLandsToVisit a =
                let (k, notLandsToVisit') = S.deleteFindMin $ notLandsToVisit a
                in Just (k, a { landsToVisit=notLandsToVisit', visited = S.insert k $ visited a} )
            | null (landsToVisit a) && null (notLandsToVisit a) = Nothing
