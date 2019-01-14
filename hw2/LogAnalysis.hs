module LogAnalysis where 

import Log

createInfoMessage :: [String] -> LogMessage
createInfoMessage (timestamp:text) = LogMessage Info (read timestamp) (unwords text)
createInfoMessage message = error ("Invalid error message" ++ unwords message) 

createWarningMessage :: [String] -> LogMessage
createWarningMessage (timestamp:text) = LogMessage Warning (read timestamp) (unwords text)
createWarningMessage message = error ("Invalid error message" ++ unwords message) 

createErrorMessage :: [String] -> LogMessage
createErrorMessage (danger:timestamp:text) = LogMessage (Error (read danger)) (read timestamp) (unwords text)
createErrorMessage message = error ("Invalid error message" ++ unwords message) 

createUnknownMessage :: [String] -> LogMessage
createUnknownMessage text = Unknown (unwords text)

parseMessage :: String -> LogMessage
parseMessage str
    | head wordList == "I" = createInfoMessage $ tail wordList
    | head wordList == "W" = createWarningMessage $ tail wordList
    | head wordList == "E" = createErrorMessage $ tail wordList
    | otherwise = createUnknownMessage $ tail wordList
    where wordList = words str
    
-- still need to deal with IO [] (list), lift the function to handle IO list or find a way to
-- convert it to a normal (make-sense-to-us-able) list    
parse :: String -> [LogMessage]
parse file = parseMessage <$> lines file

insertToTree :: LogMessage -> MessageTree -> MessageTree
insertToTree (Unknown _) tree = tree
insertToTree msg (Node left (Unknown _) right) = error "Unknown log messages are not allowed in our tree"
insertToTree msg Leaf = Node Leaf msg Leaf
insertToTree msg1@(LogMessage _ tsp1 _) (Node left msg2@(LogMessage _ tsp2 _) right)
    | tsp1 <= tsp2 = Node (insertToTree msg1 left) msg2 right
    | otherwise = Node left msg2 (insertToTree msg1 right)

buildTree :: [LogMessage] -> MessageTree
buildTree list = foldr insertToTree Leaf list

inOrderTraverse :: MessageTree -> [LogMessage]
inOrderTraverse (Node _ (Unknown _) _) = []
inOrderTraverse Leaf = []
inOrderTraverse (Node left msg right) = inOrderTraverse left ++ [msg] ++ inOrderTraverse right

isBadError :: LogMessage -> Bool
isBadError (LogMessage (Error severity) _ _) = severity > 50
isBadError _ = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong list = 
    map (\(LogMessage _ _ content) -> content)
    $ filter isBadError
    $ inOrderTraverse 
    $ buildTree list
