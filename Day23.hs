import Data.List (sortBy)
import Data.Function (on)

type Position = (Int, Int, Int)
data Bot = Bot {position :: Position, range :: Int} deriving Show

parseLine :: String -> Bot
parseLine str = let [posStr, rangeStr] = words str
                    botRange = (read . drop 2) rangeStr
                    allPos = ((drop 5) . reverse . (drop 2) . reverse) posStr 
                    botPos = read $ "(" ++ allPos ++ ")"
                in Bot botPos botRange

getData :: IO [Bot]
getData = readFile "input23.txt" >>= (return . (map parseLine) . lines)

distance :: Position -> Position -> Int
-- simple helper for computing the Manhattan distance between 2 points
distance (u, v, w) (x, y, z) = sum $ (map abs) [u-x, v-y, w-z]

inRangeOf :: Position -> Bot -> Bool
-- determines if the given position is in range of the given bot
pos `inRangeOf` bot = distance pos (position bot) <= range bot

solveFirst :: [Bot] -> Int
solveFirst bots = let largest = last $ sortBy (compare `on` range) bots
                  in length $ filter ((flip inRangeOf) largest) $ map position bots

first :: IO ()
first = getData >>= (putStrLn . show . solveFirst)

xRange :: [Bot] -> [Int]
-- given a list of bots, find the range of x values which can conceivably be in range of any of them
xRange bots = [minX..maxX]
    where xmin Bot {position=(x, _, _), range=r} = x - r
          xmax Bot {position=(x, _, _), range=r} = x + r
          minX = minimum $ map xmin bots
          maxX = maximum $ map xmax bots

yRange :: [Bot] -> [Int]
-- same, for y values
yRange bots = [minY..maxY]
    where ymin Bot {position=(_, y, _), range=r} = y - r
          ymax Bot {position=(_, y, _), range=r} = y + r
          minY = minimum $ map ymin bots
          maxY = maximum $ map ymax bots

zRange :: [Bot] -> [Int]
-- same, for z values
zRange bots = [minZ..maxZ]
    where zmin Bot {position=(_, _, z), range=r} = z - r
          zmax Bot {position=(_, _, z), range=r} = z + r
          minZ = minimum $ map zmin bots
          maxZ = maximum $ map zmax bots

numBotsInRange :: [Bot] -> Position -> Int
-- find the number of bots, from the list, in range of the given point
numBotsInRange bots pos = length $ filter (inRangeOf pos) bots

-- I stopped at this point, because (predictably), this function is much too slow, even for the test 10x10 case
-- given in the puzzle. There is no hope of computing the solution to the second puzzle in reasonable time
-- without improving the algorithem dramatically.
biggestNumInRange :: [Bot] -> Int
-- simply find the maximum number of bots in range of any point
biggestNumInRange bots = maximum $ map (numBotsInRange bots)
                            $ [(x, y, z) | x <- xRange bots, y <- yRange bots, z <- zRange bots]


