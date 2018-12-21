import qualified Data.Map.Strict as Map

type Location = (Int, Int)
type Grid = Map.Map Location Char

parseInput :: String -> Grid
-- this type of parsing is standard by now, I just copied it from Day 15 :-)
parseInput input = fst $ foldr verticalFold (Map.empty, (length horizontals) - 1) horizontals
    where horizontals = lines input
          parseLine str lineNo = fst $ foldr (horizontalFold lineNo) (Map.empty, (length str) - 1) str
          horizontalFold lineNo chr (lineMap, idx) = (Map.insert (idx, lineNo) chr lineMap, idx - 1)
          verticalFold line (finalMap, lineNo) = (Map.union finalMap (parseLine line lineNo), lineNo - 1)

getData :: IO Grid
getData = readFile "input18.txt" >>= (return . parseInput)

getNeighbours :: Location -> [Location]
getNeighbours (x, y) = let width = 50; height = 50 -- simpler to hardcode the dimensions, no reason not to here
                       in [(z, w) | z <- [0..(width-1)], w <- [0..(height-1)],
                                    abs (z - x) < 2, abs (w - y) < 2, (z, w) /= (x, y)]

evolve :: Grid -> Location -> Char
-- determine what a location becomes in the following minute
evolve grid location = case Map.lookup location grid of
                        Just '.' -> if length (filter id (map ((== Just '|') . ((flip Map.lookup) grid))
                                       (getNeighbours location))) >= 3
                                        then '|'
                                        else '.'
                        Just '|' -> if length (filter id (map ((== Just '#') . ((flip Map.lookup) grid))
                                       (getNeighbours location))) >= 3
                                        then '#'
                                        else '|'
                        Just '#' -> if (Just '#' `elem` (map ((flip Map.lookup) grid) (getNeighbours location)))
                                        && (Just '|' `elem` (map ((flip Map.lookup) grid) (getNeighbours location)))
                                        then '#'
                                        else '.'

evolveGrid :: Grid -> Grid
evolveGrid grid = Map.mapWithKey (\loc _ -> evolve grid loc) grid

evolveRepeat :: Int -> Grid -> Grid
evolveRepeat n grid = if n == 0 then grid else evolveGrid (evolveRepeat (n-1) grid)

solveFirst :: Grid -> Int
solveFirst grid = let final = evolveRepeat 10 grid
                      numOf chr = Map.size $ (Map.filter (== chr)) final
                  in (numOf '|') * (numOf '#')

first :: IO ()
first = getData >>= (putStrLn . show . solveFirst)

-- theoretical, clearly will run for centuries!
solveSecond :: Grid -> Int
solveSecond grid = let final = evolveRepeat 1000000000 grid
                       numOf chr = Map.size $ (Map.filter (== chr)) final
                   in (numOf '|') * (numOf '#')

second :: IO ()
second = getData >>= (putStrLn . show . solveSecond)

-- I have my suspicions though. Perhaps there is a repeating pattern eventually? Let's do a few evolutions
-- and see!
suspicious :: Grid -> [Grid]
suspicious grid = map (flip evolveRepeat grid) [0..]

printAndCheck :: IO ()
printAndCheck = getData >>= (\grid -> recurse grid 0)
    where recurse grid num = do
            let infinite = suspicious grid
            putStrLn $ "checking generation " ++ show num
            let candidate = infinite!!num
            putStrLn "computing previous positions"
            let previous = if num > 0 then take (num - 1) infinite else []
            putStrLn "comparing..."
            if candidate `elem` previous
                then do
                        putStrLn "match found!"
                        let match = head $ dropWhile (\idx -> infinite!!idx /= candidate) [0..]
                        putStrLn $ "agrees with generation " ++ show match
                else
                    recurse grid (num+1)

-- runs incredibly slowly (more than half an hour just for the first 10!, presumably due to generating all the
-- previous states and checking against each, at every generation). So trying a new (slighly less rigorous)
-- approach - just compute the number of each symbol at each stage, and print them out.
numericalCyclesCheck :: IO ()
numericalCyclesCheck = getData >>= (\grid -> recurse grid 0)
    where numOf chr = Map.size . (Map.filter (== chr))
          recurse grid num = do
            putStrLn $ "generation " ++ show num
            let newGrid = evolveGrid grid
            putStrLn $ "found " ++ (show $ numOf '.' grid) ++ " dots,"
            putStrLn $ (show $ numOf '|'grid) ++ " pipes,"
            putStrLn $ "and " ++ (show $ numOf '#' grid) ++ " hashes"
            -- just recurse indefinitely, will stop after some reasonable time to inspect
            recurse newGrid (num+1)
-- unfortunately, even this didn't help find any patterns, I ran for over 350 generations (nearly 2 hours!)
-- and there were no potential cycles started, and still fairly large oscillations in values. Maybe I am supposed
-- to actually compute this! (Or perhaps only as far as a few thousand when it finally setlles down into a fixed
-- or low-cycle-repeating pattern?)