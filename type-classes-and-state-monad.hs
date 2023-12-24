{-# LANGUAGE FunctionalDependencies #-}
import Control.Monad.State

class Ord k => Consumable a k | a -> k where
  consume :: a -> k -> (Maybe a, Maybe k)

consumeState :: (Consumable a k, Ord k) => k -> State a (Maybe k)
consumeState k = do
  s <- get
  case consume s k of
    (_, Just _) -> return $ Just k
    (Just s', Nothing) -> do
                      put s'
                      consumeState k
    (Nothing, Nothing) -> return Nothing


instance Ord a => Consumable [a] a where
  consume [] _ = (Nothing, Nothing)
  consume (x:xs) y 
    | x == y = (Just xs, Just x)
    | otherwise = (Just xs, Nothing)

main :: IO ()
main = do 
  let x = evalState (consumeState 11) [2, 11, 3]  
  print x
