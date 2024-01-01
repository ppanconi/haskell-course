{-# LANGUAGE FunctionalDependencies #-}
module VisitableLands where
    import Control.Monad.State

    class Logbook a k | a -> k where
        isNotVisited :: a -> k -> Bool
        registerVisited :: a -> k -> a
        stackedToVisit :: a -> [k]
        stackToVisit :: a -> k -> a

    class VisitableLands l k | k -> l where
        isLand :: l -> k -> Bool
        neighboringDistricts :: l -> k -> [k]
        updateSate :: Logbook s k => k -> State s s
        updateSate k = do
            s <- get
            let s' = registerVisited s k in do
                put s'
                return s'
        exploreLand :: Logbook s k => l -> k -> State s ()
        exploreLand l k = do
            updateSate k
            mapM_ exploreNeighborLand (neighboringDistricts l k)
            where exploreNeighborLand _k = do
                    s' <- get
                    when (isNotVisited s' _k) $ 
                        if isLand l _k then
                            exploreLand l _k
                        else do
                            put $ stackToVisit s' _k
        searchLands :: Logbook s k => l -> k -> State s Int
        searchLands l k = do
            updateSate k
            -- if the starting element in visit is a land we need to count it        
            n <- if isLand l k then do
                                    exploreLand l k
                                    return 1
                                else return 0
            -- we exam k neighboringDistricts
            n' <- foldM exploreNeighbor n (neighboringDistricts l k)
            s <- get
            foldM exploreNeighbor n' (stackedToVisit s)
            where exploreNeighbor _n _k = do
                    s' <- get
                    if isNotVisited s' _k then
                        if isLand l _k then do
                            exploreLand l _k
                            return $ if isLand l k then _n else _n + 1
                        else do
                            -- _k is not a land
                            n' <- searchLands l _k
                            return (_n + n')
                    else return _n