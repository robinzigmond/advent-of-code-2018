import Data.Char (ord)
import Data.List (sort)

parseStr :: String -> (Char, Char)
-- decodes the input into a sequence of pairs, where (A,B)
-- denotes that A must happen before B
parseStr str = (str!!5, str!!36)

getData :: IO [(Char, Char)]
getData = do
    input <- readFile "input7.txt"
    return $ map parseStr $ lines input

type Prerequisites a = (a -> [a])
-- this type synonym represents a function which, given a value representing a task,
-- returns a list of those tasks which must be done first

requirementsList :: (Eq a) => [(a, a)] -> Prerequisites a
-- given a list of pairs (x, y) (representing "x must be done before y can"),
-- determine the list of prerequesites in the above form
requirementsList pairs x = [y | (y, z) <- pairs, z == x]

nextTask :: (Ord a) => [a] -> Prerequisites a -> [a] -> a
-- given a set of values, prerequisites for them, and a list of already done tasks, figures out the
-- next task which should be done. (The first, in the natural order, of those which have all their
-- prerequsities fulfilled.)
nextTask xs f ys = minimum $ filter allowed xs
    where allowed x = not (x `elem` ys) && (all (\y -> y `elem` ys) $ f x)

solveFirst :: String -> [(Char, Char)] -> String
-- inputs: the list of all chars used (here A-Z), and the prerequisite pairs.
-- Returns the correct sequence.
-- uWes an auxiliary recursive function to build up the list
solveFirst chars pairs = reverse $ auxFunc chars pairs ""
    where auxFunc chars prs done
            | length done >= length chars = done
            | otherwise = auxFunc chars prs $ (nextTask chars (requirementsList prs) done) : done

first :: IO ()
first = getData >>= (putStrLn . solveFirst ['A'..'Z'])

timeToDo :: Char -> Int
timeToDo x = ord x - 4

canDoNext :: (Eq a) => [a] -> Prerequisites a -> [a] -> [a] -> [a]
-- "reverses" the prerequsites function, in a sense. Takes a list of all concerned tasks,
-- a prerequisites mapping, a list of tasks all done, and one of those currently underwya.
-- Returns the list of all tasks which are able to be started next

-- We've basically already done this, in the nextTask function above. The only differences are that
-- rather than taking the minimum of the set of allowed values (and therefore requiring them to
-- be ordered), we are now interested in the whole list - and that, since every task takes time,
-- we separate the already finished tasks from those still underway
canDoNext vals f done started = filter allowed vals
    where allowed x = not (x `elem` done || x `elem` started) && (all (\y -> y `elem` done) $ f x)

-- Type to keep track of the progress of all the tasks each second (the Int is the seconds remaining)
type TaskState a = [(a, Int)]

-- we will define some simple utility functions on the state
finished :: TaskState a -> [a]
finished xs = map fst $ filter ((== 0) . snd) xs

started :: TaskState Char -> [Char]
started xs = map fst $ filter (\(x, y) -> (y > 0) && (y < timeToDo x)) xs

initialState :: TaskState Char
initialState = map (\char -> (char, timeToDo char)) ['A'..'Z']

-- the following function defines the main logic for running this state machine
oneStateRun :: [Char] -> Prerequisites Char -> TaskState Char -> TaskState Char
oneStateRun xs f currState = newState
    where done = finished currState
          running = started currState
          timeLeft x = head $ map snd $ filter (\(y, z) -> y == x) currState
          numIdleElves = 5 - (length $ filter ((> 0) . timeLeft) running)
          nextToStart = reverse $ sort $ canDoNext xs f done running
          -- feels like it's best in general to start the longest-running task first. Not sure if always
          -- true though? (It worked in this case, anyway!)
          aboutToRun = case (numIdleElves > length nextToStart) of
                        True -> nextToStart
                        False -> map (\idx -> nextToStart!!idx) [0..numIdleElves-1]
          newTime x = if x `elem` running
                        then max 0 (timeLeft x - 1)
                        else if x `elem` aboutToRun
                            then timeLeft x - 1
                            else timeLeft x
          newState = map (\x -> (x, newTime x)) xs

isDone :: TaskState Char -> Bool
isDone = all ((<= 0) . snd) 

solveSecond :: String -> [(Char, Char)] -> Int
solveSecond xs pairs = answer initialState
    where prereqs = requirementsList pairs
          answer state = if isDone state
                            then 0
                            else 1 + (answer $ oneStateRun xs prereqs state)

second :: IO ()
second = getData >>= (putStrLn . show . solveSecond ['A'..'Z'])
