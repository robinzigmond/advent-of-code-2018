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

{- clearly far too slow to run in reasonable time, so I haven't tried! Computing 5000 generations takes about 11
seconds currently, 50000 about 2 minutes, so it would appear to need weeks to get to 50 billion! Clearly I need
an algorithem which is MUCH quicker than O(n), which my current one is, due to the folds. But I am unable to come
up with any way of computing one generation from the next without stepping through the map (or list, or whatever)
one element at a time, which will always be O(n)... -}
solveSecond :: Pots -> Rules -> Int
solveSecond pots rules = sum hasPots
    where finalPots = evolve pots rules 50000000000
          hasPots = filter (\key -> (finalPots Map.! key) == '#') (Map.keys finalPots)

second :: IO ()
second = getData >>= (putStrLn . show . (uncurry solveSecond))
