import qualified Data.Map.Strict as Map
import Data.Text (splitOn, pack, unpack)
import Data.List (sort)

type Location = (Int, Int)
type Grid = Map.Map Location Char

spring :: Location
spring = (500, 0)

-- first, some functions to parse the input data
parseLine :: String -> Grid
parseLine str = foldr (\loc map -> Map.insert loc '#' map) Map.empty locations
    where parts = words str
          firstCoord = head str
          value = read $ init $ drop 2 $ parts!!0 :: Int
          strRange = drop 2 $ parts!!1
          [minRange, maxRange] = map unpack $ splitOn (pack "..") (pack strRange)
          range = [(read minRange)..(read maxRange)] :: [Int]
          locations = if firstCoord == 'x'
                        then [(value, y) | y <- range]
                        else [(x, value) | x <- range]

parseFully :: [String] -> Grid
-- use the above function to parse all of the lines
parseFully = foldr (\str map -> Map.union map $ parseLine str) Map.empty

getData :: IO Grid
getData = (readFile "input17.txt") >>= (return . parseFully . lines)


topBound :: Grid -> Int
topBound = minimum . (map snd) . Map.keys

bottomBound :: Grid -> Int
bottomBound = maximum . (map snd) . Map.keys

-- now, some functions for modelling water flow

findLowerBound :: Grid -> Location -> Maybe Location
-- given a grid and a starting point for water to start falling, finds the point at which
-- downward water flow will stop (a # or ~), if it exists
findLowerBound grid (x, y) = let boundary = [(x, z) | z <- [(topBound grid)..(y-1)],
                                                        Map.lookup (x, z) grid `elem` [Just '#', Just '~'] ]
                             in if null boundary then Nothing else Just $ last $ sort boundary

