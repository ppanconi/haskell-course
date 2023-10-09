import Distribution.Simple.Utils (xargs)
-- Applicative functors, (ii)

-- :k Functor
-- Functor :: * -> *

-- Functor
-- class Functor f where
--    fmap :: (a -> b) -> f a -> f b     -- <$>
-- Applicative 
-- class Functor f => Applicative f where
--    pure :: a -> f a
--    (<*>)  :: f (a -> b) -> f a -> f b -- apply or ap
---
--- h <$> f == pure h <*> f
--- 

-- List [] -- Employee, [Name], [String] -> [Employee]

-- instance Functor [] where
--     fmap _ []   = []
--     fmap f (x:xs) = f x ++ fmap f xs

-- instance Applicative [] where
--     pure a = [a]
--     [] (<*>) _ = []
--     (h:hs) (<*>) fs = map h fs ++ hs (<*>) fs 

names  = ["Joe", "Sara", "Mae"]
phones = ["555-5555", "123-456-7890", "555-4321"]

-- employees1 = Employee <$> names <*> phones

