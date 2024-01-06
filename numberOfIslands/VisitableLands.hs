{-# LANGUAGE FunctionalDependencies #-}
module VisitableLands where
    import Control.Monad.State

    class Logbook a k | a -> k where
        isNotVisited :: a -> k -> Bool
        registerVisited :: a -> k -> a
        stackedToVisit :: a -> [k]
        stackToVisit :: a -> k -> a

    class VisitableLands l k where
        isLand :: l -> k -> Bool
        neighboringDistricts :: l -> k -> [k]
        exploreLand :: Logbook s k => l -> k -> State s ()
        exploreLand l k = do
            updateSate k
            mapM_ exploreNeighborLand (neighboringDistricts l k)
            where exploreNeighborLand k' = do
                    s' <- get
                    when (isNotVisited s' k') $ 
                        if isLand l k' then
                            exploreLand l k'
                        else
                            put $ stackToVisit s' k'
        searchLands :: Logbook s k => l -> k -> State s Int
        searchLands l k = do
            -- if the starting element in visit is a land we need to count it        
            n <- if isLand l k then do
                                    exploreLand l k
                                    return 1
                               else do 
                                    updateSate k
                                    return 0
            -- we exam k neighboringDistricts
            n <- foldM exploreNeighbor n (neighboringDistricts l k)
            s <- get
            foldM exploreNeighbor n (stackedToVisit s)
            where exploreNeighbor n' k' = do
                    s' <- get
                    if isNotVisited s' k' then
                        if isLand l k' then do
                            exploreLand l k'
                            return $ if isLand l k then n' else n' + 1
                        else do
                            -- _k is not a land
                            n'' <- searchLands l k'
                            return (n' + n'')
                    else return n'

    updateSate :: Logbook s k => k -> State s s
    updateSate k = do
        s <- get
        let s' = registerVisited s k 
        put s'
        return s'