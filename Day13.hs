import qualified Data.Map.Strict as Map
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Tuple (swap)
import Control.Monad.State.Lazy

type Tracks = Map.Map (Int, Int) Char
data CartNextMove = TurnLeft | GoStraight | TurnRight deriving (Eq, Enum, Bounded, Show)
type CartDirection = Char
type CartState = ((Int, Int), CartDirection, CartNextMove)
type PuzzleState = (State [CartState]) [(Int, Int)]

parseInput :: String -> (Tracks, [CartState])
-- hideously ugly parse function. I actually managed to get the (x, y) key pairs the wrong way round
-- and only noticed later when trying to run the solution - but given how complicated the parse function
-- is, it was easiest to leave it unchanged and alter the rest of the code where necessary.
-- (This includes reversing the output when printing to the screen.)
parseInput trackData = (tracks, carts)
    where (tracks, carts) = snd $ foldr verticalFold ((length horizontals) - 1, (Map.empty, [])) horizontals
          horizontals = lines trackData
          parseLine str lineNo = snd $ foldr (horizFold lineNo) ((length str) - 1, (Map.empty, [])) str
          horizFold lineNo char (num, (map, cartInfo)) = if isSpace char
                                        then (num - 1, (map, cartInfo))
                                        else if char `elem` ['^', 'v']
                                            then (num - 1, ((Map.insert (lineNo, num) '|' map),
                                                ((lineNo, num), char, TurnLeft) : cartInfo))
                                            else if char `elem` ['<', '>']
                                                then (num - 1, ((Map.insert (lineNo, num) '-' map),
                                                    ((lineNo, num), char, TurnLeft) : cartInfo))
                                                else (num - 1, ((Map.insert (lineNo, num) char map), cartInfo))

          verticalFold line (num, (map, carts))
            = (num - 1,
                ((map `Map.union` (fst (parseLine (horizontals!!num) num))),
                carts ++ snd (parseLine (horizontals!!num) num)))

getData :: IO (Tracks, [CartState])
getData = readFile "input13.txt" >>= (return . parseInput)

updateState :: Tracks -> CartState -> CartState
-- update the position and direction of a single cart, according to the rules
updateState trackMap ((y, x), dir, nextMove) = ((newY, newX), newDir, newMove)
    where newX = case dir of
                    '>' -> x + 1
                    '<' -> x - 1
                    _ -> x
          newY = case dir of
                    'v' -> y + 1
                    '^' -> y - 1
                    _ -> y
          newDir = case trackMap Map.! (newY, newX) of
                    '-' -> dir -- must have already been travelling the same direction
                    '|' -> dir
                    '/' -> case dir of
                            '^' -> '>'
                            'v' -> '<'
                            '>' -> '^'
                            '<' -> 'v'
                    '\\' -> case dir of
                            '^' -> '<'
                            'v' -> '>'
                            '>' -> 'v'
                            '<' -> '^'
                    '+' -> case nextMove of
                            TurnLeft -> case dir of
                                            '^' -> '<'
                                            'v' -> '>'
                                            '>' -> '^'
                                            '<' -> 'v'
                            GoStraight -> dir
                            TurnRight -> case dir of
                                            '^' -> '>'
                                            'v' -> '<'
                                            '>' -> 'v'
                                            '<' -> '^'
          newMove = if trackMap Map.! (newY, newX) == '+'
                        then if nextMove == maxBound
                            then minBound :: CartNextMove
                            else succ nextMove
                        else nextMove

hasCollision :: [CartState] -> [(Int, Int)]
-- determine if any collision have occured, and if so get their locations
hasCollision = collisionLocation . map (\(pos, _, _) -> pos)
    where collisionLocation [] = []
          collisionLocation (x:xs) = if x `elem` xs
                                        then x : (collisionLocation xs)
                                        else collisionLocation xs

getCollision ::  Tracks -> [CartState] -> [(Int, Int)]
-- tests to see if moving all carts by one step will cause any collisions. Returns a list of
-- the collision location if there will be any
getCollision tracks carts =
    let collision = hasCollision $ map (updateState tracks) carts
    in if not $ null collision
        then collision -- will be a collision if any 2 carts move to the same place
        else let getPos (pos, _, _) = pos
                 colls = [(cart1, cart2) | cart1 <- carts, cart2 <- carts,
                            getPos cart1 < getPos cart2,
                            getPos (updateState tracks cart1) == getPos cart2]
             in if null colls then [] else map (getPos . snd) colls

processor :: Tracks -> PuzzleState
-- use the above functions to define the crucial state-processing function and put it into the State monad
processor tracks = state $ \carts -> (getCollision tracks carts, bulkUpdate tracks carts)
    where bulkUpdate = map . updateState

collisionState :: Tracks -> PuzzleState
-- runs the state computation until a collision is about to occur
collisionState tracks = do
    collision <- processor tracks
    if null collision
        then collisionState tracks
        else return collision

solveFirst :: Tracks -> [CartState] -> (Int, Int)
solveFirst tracks initialState = x
    where (x:xs) = evalState (collisionState tracks) initialState

first :: IO ()
first = do
    (tracks, carts) <- getData
    putStrLn $ show $ swap $ solveFirst tracks carts

removeCollided :: Tracks -> [CartState] -> [CartState]
-- checks if any carts are about to collide and, if so, removes them from the computation
removeCollided tracks carts =
    let collisions = getCollision tracks carts
        getPos (pos, _, _) = pos
    in filter (\cart -> not (((getPos . (updateState tracks)) cart `elem` collisions)
        || getPos cart `elem` collisions)) carts

goBack :: Tracks -> CartState -> CartState
-- hacky function, required to "rewind" the state one step, due to mismatch
-- in how the timing works (removeCollided only removes carts which could collide in the *following* tick)
goBack trackMap ((y, x), dir, nextMove) = ((oldY, oldX), oldDir, oldMove)
    where oldX = case oldDir of
                    '>' -> x - 1
                    '<' -> x + 1
                    _ -> x
          oldY = case oldDir of
                    'v' -> y - 1
                    '^' -> y + 1
                    _ -> y
          oldDir = case trackMap Map.! (y, x) of
                    '-' -> dir
                    '|' -> dir
                    '/' -> case dir of
                            '^' -> '>'
                            'v' -> '<'
                            '>' -> '^'
                            '<' -> 'v'
                    '\\' -> case dir of
                            '^' -> '<'
                            'v' -> '>'
                            '>' -> 'v'
                            '<' -> '^'
                    '+' -> case oldMove of
                            TurnLeft -> case dir of
                                            '^' -> '>'
                                            'v' -> '<'
                                            '>' -> 'v'
                                            '<' -> '^'
                            GoStraight -> dir
                            TurnRight -> case dir of
                                            '^' -> '<'
                                            'v' -> '>'
                                            '>' -> '^'
                                            '<' -> 'v'
          oldMove = if trackMap Map.! (y, x) == '+'
                        then if nextMove == minBound
                            then maxBound :: CartNextMove
                            else pred nextMove
                        else nextMove
                    
stateAtCollision :: Tracks -> [CartState] -> [CartState]
-- runs the state process forward until one or more collisions occurs, removes the carts which
-- collided, and returns the new state
stateAtCollision tracks = (removeCollided tracks) . (map (goBack tracks)) . (execState (collisionState tracks))

solveSecond :: Tracks -> [CartState] -> (Int, Int)
solveSecond tracks initialState =
    let collisionResult = stateAtCollision tracks initialState
    in if length collisionResult > 1
        then solveSecond tracks collisionResult
        else (\(pos, _, _) -> pos) $ head $ map (updateState tracks) $ collisionResult

second :: IO ()
second = do
    (tracks, carts) <- getData
    putStrLn $ show $ swap $ solveSecond tracks carts
