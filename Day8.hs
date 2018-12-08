import Data.List (delete, sortBy)
import Data.Function (on)

getData :: IO [Int]
getData = (readFile "input8.txt") >>= (return . (map read) . words)

data Header = Header {numChildren :: Int, numMeta :: Int} deriving (Eq, Show)
data Node = Node {label :: Int, header :: Header, children :: [Node], metadata :: [Int]} deriving (Show)

-- 2 nodes should be considered equal whenever they have the same label
instance Eq Node where
    nd1 == nd2 = (label nd1 == label nd2)

-- the key to the puzzle is to parse the input (list of Ints) into a list of Nodes
-- to do this we need to build up our list gradually, and keep track of state (such as
-- where we are in the input list, and which partially-parsed node we are currently in)

-- first we need to define a type to represent the parse state
data ParseState = ParseState {inputLeft :: [Int], completeNodesSoFar :: [Node],
                                currentNode :: Node, parentsNotComplete :: [Node]} deriving (Show)

-- and the initial state (depends on the list we read from the file)

emptyNode :: Int -> Node
emptyNode n = Node n (Header (-1) (-1)) [] []
-- (since all actual values are non-negative, we initialise both parts of the header to -1 to signal
-- that they still need to be filled)

getInitialState :: [Int] -> ParseState
getInitialState xs = ParseState xs [] (emptyNode 1) []

isParsingComplete :: ParseState -> Bool
isParsingComplete state = (null $ inputLeft state) && (null $ parentsNotComplete state)

-- some utility functions for dealing with nodes:

isParent :: Node -> Node -> Bool
isParent nd1 nd2 = nd2 `elem` children nd1

hasHeader :: Node -> Bool
-- determines if the "header" portion of a node has been fully parsed yet.
hasHeader node = (numChildren (header node) > -1) && (numMeta (header node) > -1)

childrenComplete :: Node -> Bool
childrenComplete node = length (children node) == (numChildren $ header node)

metadataComplete :: Node -> Bool
metadataComplete node = length (metadata node) == (numMeta $ header node)

isEmptyNode :: Node -> Bool
isEmptyNode nd = header nd == (Header (-1) (-1))

isComplete :: Node -> Bool
-- determine if a node is valid or not (if not, it needs further parsing)
isComplete node = all id $ [hasHeader, childrenComplete, metadataComplete] <*> [node]

-- here comes the key function which reads the next digit of the input and updates our state

-- Parsing algorithm in detail (it will make the code below easier to follow, not to mention easier
-- for me to write :-) ):
-- 1) when current node is empty, enter the number into the header's numChildren property. (Nothing else changes)
-- 2) when it's not empty, but the header isn't yet complete (has a -1), enter the number as numMeta
-- 3) if the header is complete, check how many children the node has in the completeSoFar part of the state
-- 4) if this is less than node's header's numChildren property, create a new child and move into it
-- (go back to 1), without moving on from the current number
-- 5) if the children are all complete, and the length of the metadata property is equal to the
-- numMeta header element, complete that node and move back up to the parent which we came from (this will be
-- the most recently-added node in the alsoStarted property). Again stay on the same number.
-- 6) if the children are complete, but the metadata isn't, add the number to the metadata 
-- I've actually chosen to follow the above steps so closely that I have made a local function "stepNumber"
-- which simply says which of the numbered cases above we are in. (This is why 3 is not returned in any case.)
nextState :: ParseState -> ParseState
nextState (ParseState [] completeSoFar currentNode alsoStarted)
    = ParseState [] (currentNode : completeSoFar) currentNode []
nextState (ParseState (x:xs) completeSoFar currentNode alsoStarted)
    = ParseState newInput parsedNodes nowCurrent nowUnderway
    where stepNumber = if isEmptyNode currentNode
                        then 1 
                        else if not $ hasHeader currentNode
                            then 2
                            else if childrenComplete currentNode
                                    then if metadataComplete currentNode
                                        then 5
                                        else 6
                                    else 4
          nextLabel = 1 + (maximum $ map label $ [currentNode] ++ completeSoFar ++ alsoStarted)
          newInput = case stepNumber of
                        1 -> xs
                        2 -> xs
                        4 -> x:xs
                        5 -> x:xs
                        6 -> xs
          parsedNodes = case stepNumber of
                            1 -> completeSoFar
                            2 -> completeSoFar
                            4 -> completeSoFar
                            5 -> currentNode : completeSoFar
                            6 -> completeSoFar
          nowCurrent = case stepNumber of
                        1 -> Node (label currentNode) (Header x (-1)) [] []
                        2 -> Node (label currentNode) (Header (numChildren $ header currentNode) x) [] []
                        4 -> emptyNode nextLabel
                        5 -> head $ filter (\nd -> nd `isParent` currentNode) alsoStarted
                        6 -> Node (label currentNode) (header currentNode)
                                (children currentNode) (x : metadata currentNode)
          nowUnderway = case stepNumber of
                            1 -> alsoStarted
                            2 -> alsoStarted
                            4 -> (Node (label currentNode) (header currentNode)
                                    ((emptyNode nextLabel) : (children currentNode))
                                    (metadata currentNode)) : alsoStarted
                            5 -> delete currentNode alsoStarted
                            6 -> alsoStarted

getNodeList :: [Int] -> [Node]
-- performs the full parsing process, going from the initial list of numbers to the list of nodes
getNodeList = completeNodesSoFar . doParse . getInitialState
            where doParse state = if isParsingComplete state then state else doParse $ nextState state

solveFirst :: [Int] -> Int
solveFirst = sum . (concatMap metadata) . getNodeList

first :: IO ()
first = getData >>= (putStrLn . show . solveFirst)

-- The next 2 functions are a bit of a hack, which is the simplest way to fix
-- as error I made with the above state/parsing function. It worked for puzzle 1,
-- because it ensures the right metadata is collected for each node, it does not track the
-- children properly (the children property has the correct IDs but leaves them otherwise with the
-- properties of an empty node). And this is no good for the recursive function below.

-- The easiest fix, since the IDs agree, is to get the "real" children and metadata by
-- looking up the node of that ID at the top level. Hence these functions (recall that
-- we defined nodes to be equal according to the == operator whenever their IDs agree)
getRealChildren :: [Node] -> Node -> [Node]
getRealChildren nodes node = children $ head $ filter (== node) nodes

getRealMetadata :: [Node] -> Node -> [Int]
getRealMetadata nodes node = metadata $ head $ filter (== node) nodes

value :: [Node] -> Node -> Int
-- computes the "value" of a node, recursively, according to the definition given in the second
-- part of the puzzle
value nodes node = case (length $ getRealChildren nodes node) of
                    0 -> sum $ getRealMetadata nodes node
                    _ -> let orderedChildren = sortBy (compare `on` label) $ getRealChildren nodes node
                             valueOfChild num = if num > (length $ getRealChildren nodes node)
                                                    then 0
                                                    else value nodes $ orderedChildren!!(num - 1)
                         in sum $ map valueOfChild $ getRealMetadata nodes node

rootNode :: [Node] -> [Node]
-- find all nodes in the list which are not a child of any others
-- (The puzzles assures us that the resulting list will have length one)
rootNode nodes = filter (\node -> not $ node `elem` (concatMap (getRealChildren nodes) nodes)) nodes

solveSecond :: [Int] -> Int
solveSecond nums = let nodes = getNodeList nums
                   in value nodes $ head $ rootNode $ nodes

second :: IO ()
second = getData >>= (putStrLn . show . solveSecond)
