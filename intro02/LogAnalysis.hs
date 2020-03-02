{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- parseMessage --
parseMessage :: String -> LogMessage
parseMessage s
   | head w == "E" = LogMessage (Error pos1) pos2 (unwords (drop 3 w))
   | head w == "I" = LogMessage Info pos1 (unwords (drop 2 w))
   | otherwise = Unknown s
   where pos1 = read (w !! 1) :: Int
         pos2 = read (w !! 2) :: Int
         w = words s

-- parse --
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- insert --
insert :: LogMessage
       -> MessageTree
       -> MessageTree
insert (Unknown _) tree = tree
insert msg Leaf = Node Leaf msg Leaf
insert msgNew@(LogMessage _ tsNew _) (Node left msgOld@(LogMessage _ tsOld _) right)
   | tsOld <= tsNew = Node left msgOld (insert msgNew right)
   | otherwise = Node (insert msgNew left) msgOld right

-- build --
build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x $ build xs

-- inOrder --
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left msg right) = inOrder left ++ [msg] ++ inOrder right

-- whatWentWrong --
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = getString <$> filter isEmergency (inOrder $ build logs)

-- isEmergency --
isEmergency :: LogMessage -> Bool
isEmergency (LogMessage (Error val) _ _) = val >= 50
isEmergency _ = False

-- getString --
getString :: LogMessage -> String
getString (LogMessage _ _ s) = s
getString (Unknown s) = s
