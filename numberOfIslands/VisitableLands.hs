{-# LANGUAGE FunctionalDependencies #-}
module VisitableLands where
    import Control.Monad.State

    class Logbook a k | a -> k where
        justBegun :: a -> Bool
        isUnknownDistrict :: a -> k -> Bool
        isThereALandToVisit :: a -> Bool
        addALandToVisit :: a -> k -> a
        addANotLandToVisit :: a -> k -> a
        next:: a -> k -> Maybe (k, a)

    class VisitableLands l k where
        isLand :: l -> k -> Bool
        neighboringDistricts :: l -> k -> [k]
        searchLands :: Logbook s k => l -> k -> State s Int
        searchLands l k = do
            s <- get
            let neighbors = neighboringDistricts l k
                -- if the starting element in visit is a land we need to count it
                countStartingDistrictIfLand = if isLand l k && justBegun s then 1 else 0
                -- we evaluate current element neighbors avoiding already know elements.
                (s', n') = foldr evaluateNeighbor (s, countStartingDistrictIfLand) $ filter (isUnknownDistrict s) neighbors
                    where
                        evaluateNeighbor _k (_s, _n)
                            | isLand l _k && not (isThereALandToVisit _s) && not (isLand l k) = (addALandToVisit _s _k, 1)
                            | isLand l _k = (addALandToVisit _s _k, _n)
                            | otherwise = (addANotLandToVisit _s _k, _n)
                in case next s' k of
                    Nothing -> do
                        put s'
                        return n'
                    Just (k', s'') -> do
                        put s''
                        n'' <- searchLands l k'
                        return (n' + n'')