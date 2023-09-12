module LogAnalysis where
import Log

parseMessage :: String -> LogMessage
parseMessage msg = case ws of
    "I" : t : xs        -> LogMessage Info (read t) (unwords xs)
    "W" : t : xs        -> LogMessage Warning (read t) (unwords xs)
    "E" : s : t : xs    -> LogMessage (Error (read s)) (read t) (unwords xs)
    _                   -> Unknown msg
    where ws = words msg

parse :: String -> [LogMessage]
parse ls = map parseMessage (lines ls)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m@(LogMessage {}) Leaf = Node Leaf m Leaf
insert m@(LogMessage _ ts _) (Node tl n@(LogMessage _ tsn _) tr) =
    if ts <= tsn
    then Node (insert m tl) n tr
    else Node tl n (insert m tr)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node tl m tr) = inOrder tl ++ [m] ++ inOrder tr

filterError :: [LogMessage] -> Int -> [LogMessage]
filterError [] _ = []
filterError (m@(LogMessage (Error sm) _ _) : ms) s = 
    if sm >= s 
    then m : filterError ms s
    else filterError ms s 
filterError (_ : ms) s = filterError ms s 

whatWetWrong :: [LogMessage] -> [String]
whatWetWrong ms = map extractMsg (inOrder (build (filterError ms 50)))
    where extractMsg (LogMessage _ _ m) = m
