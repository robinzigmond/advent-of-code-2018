import qualified Data.Map.Strict as Map

type Pots = Map.Map Int Char
type Rules = Map.Map String Char

getData :: IO (Pots, Rules)
-- get the initial data, in the form of:
-- a map from integers to chars, representing the initial state,
-- a map from strings to chars, representing the rules to follow
-- when generating the new pattern from the old
getData = do
    input <- readFile "input12.txt"
    let theLines = lines input
    let startString = map (\idx -> theLines!!0!!idx) [15..114]
    let startMap = foldr (\idx result -> Map.insert idx (startString!!idx) result) Map.empty [0..99]
    let instructions = map (\idx -> theLines!!idx) [2..33]
    let instructionMap = foldr (\str result -> Map.insert (map (\idx -> str!!idx) [0..4]) (str!!9) result)
                        Map.empty instructions
    return (startMap, instructionMap)

trimMap :: Pots -> Pots
-- an optimisation, to remove any '.'s from the map
trimMap = fst . (Map.partitionWithKey (\_ a -> a == '#'))

generation:: Pots -> Rules -> Pots
generation pots rules = trimMap $ foldr insertion Map.empty [leftEnd..rightEnd]
    where leftEnd = (minimum $ Map.keys pots) - 2
          rightEnd = (maximum $ Map.keys pots) + 2
          insertion key map = Map.insert key (result key) map
          result key = rules Map.! map (\idx -> Map.findWithDefault '.' idx pots) [(key-2)..(key+2)]

evolve :: Pots -> Rules -> Int -> Pots
-- calculate the given number of generations into the future
evolve pots rules gens = nextGen pots rules 0
    where nextGen pots rules num
            | num == gens = pots 
            | otherwise = nextGen (generation pots rules) rules (num + 1)

solveFirst :: Pots -> Rules -> Int
solveFirst pots rules = sum hasPots
    where finalPots = evolve pots rules 20
          hasPots = filter (\key -> (finalPots Map.! key) == '#') (Map.keys finalPots)

first :: IO ()
first = getData >>= (putStrLn . show . (uncurry solveFirst))

-- There might be a repeating pattern in the output, since 50 billion generations is clearly infeasible.
-- So let's make an infinite list of all possible outputs, in order to detect the first repeat

allStates :: Pots -> Rules -> [Pots]
allStates pots rules = pots : (map ((flip generation) rules) (allStates pots rules))

prettyDisplay :: Pots -> String
prettyDisplay pots = let leftEnd = minimum $ Map.keys pots
                         rightEnd = maximum $ Map.keys pots
                     in map (flip (Map.findWithDefault '.') pots) [leftEnd..rightEnd]

-- now show the list, and stop when a repetition is reached 
showThings :: IO ()
showThings = getData >>= (lookForRepeats . (uncurry allStates))
    where lookForRepeats [] = error "empty list!"
          lookForRepeats xs = doIt 0 xs
          doIt n xs = if xs!!n `elem` (take n xs)
                        then do
                            putStrLn "cycle detected!"
                            return ()
                        else do
                            putStrLn $ prettyDisplay $ xs!!n
                            doIt (n+1) xs

-- it works! At least in the sense that a stable pattern is clearly visible. But for some reason it isn't
-- checking for repetition properly, as it just keeps repeating without printing the "cycle detected"
-- message.

-- and after checking the rules to confirm that this map is stable, I realise the reason. The map isn't
-- "the same", because the indexes are different. The pots just "move along" by a fixed amount,
-- without any being added or removed. This allows us to easily work out how what the digit sum is
-- at any given generation, past the point at which this happens.

-- the following is a "cheaty" way to figure this out.

showCycleData :: IO ()
showCycleData = getData >>= (lookForRepeats . (uncurry allStates))
    where lookForRepeats [] = error "empty list!"
          lookForRepeats xs = doIt 0 xs
          doIt n xs = do
                        putStrLn $ "generation " ++ (show n)
                        let hasPots pots = filter (\key -> ((xs!!n) Map.! key) == '#') (Map.keys pots)
                        putStrLn $ show $ sum $ hasPots (xs!!n)
                        doIt (n+1) xs

-- the output data, together with the earlier run, shows that the sum at generation n is "eventually" equal to
-- 67n (from generation 101 onwards). So the answer is 67*50000000000 = 3350000000000 :-)
