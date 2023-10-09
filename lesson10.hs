type Name = String
type Tit = String

data Employee = Employee { name    :: Name
                         , phone   :: String }
                deriving Show

me = Employee "Paul, Panks" "453583"

-- fmap2 :: Functor f => (a -> b -> c) -> (f a -> f b -> f c)
liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
-- fmap  :: (a -> b) -> f a -> f b
-- pure  :: a -> f a
-- (<*>) :: f (a -> b) -> f a -> f b
-- liftA2' h fa fb =  fmap h fa <*> fb -- >> f (b -> c) <*> fb >> f c
-- fmap = (<$>)
liftA2' h fa fb =  h <$> fa <*> fb

-- Example
m_name1, m_name2 :: Maybe Name
m_name1 = Nothing
m_name2 = Just "Brent"

m_phone1, m_phone2 :: Maybe String
m_phone1 = Nothing
m_phone2 = Just "555-1234"

emp1 = liftA2' Employee m_name1 m_phone1
emp1' = Employee <$> m_name1 <*> m_phone1

-- we impose the followinsaag functional
-- h <$> f === pure g <*> f

data Director = Director {tit :: Tit
                          , dirName :: Name
                          , dirPhone :: String 
                         }
                         deriving Show

m_tit1 = Just "Dr."
m_tit2 = Nothing

dir1 = Director <$> m_tit1 <*> m_name1 <*> m_phone1
dir2 = Director <$> m_tit1 <*> m_name2 <*> m_phone2

-- fmap3 :: (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3' :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3' h fa fb fc = h <$> fa <*> fb <*> fc

dir3 = liftA3' Director m_tit2 m_name2 m_phone2
