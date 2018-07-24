{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage e = case (words e) of
    ("E" : severity : timestamp : msg) ->
        LogMessage (Error (read severity :: Int))
                   (read timestamp :: Int)
                   (unwords msg)
    ("W" : timestamp : msg) ->
        LogMessage Warning 
                   (read timestamp :: Int)
                   (unwords msg)
    ("I" : timestamp : msg) ->
        LogMessage Info
                   (read timestamp :: Int)
                   (unwords msg)
    _ -> Unknown e

parse :: String -> [LogMessage]
parse = fmap parseMessage . lines 

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) t = t
insert m Leaf = Node Leaf m Leaf
insert _ old@(Node _ (Unknown _) _) = old
insert m@(LogMessage _ ts _) old@(Node l n@(LogMessage _ nodets _) r)
    | ts < nodets = Node l n (insert m r)
    | ts > nodets = Node (insert m l) n r
    | otherwise = old

build :: [LogMessage] -> MessageTree
build m = foldr insert Leaf m 

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l m r) = inOrder l ++ [m] ++ inOrder r

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong m = [n | (LogMessage _ _ n) <- (inOrder . build) (filter isImportant m)]
    where isImportant (LogMessage (Error bad) _ _) = bad >= 50
          isImportant _ = False


