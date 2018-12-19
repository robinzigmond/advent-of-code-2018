{- still unsolved (even part 1), but not because I've given up! It's just a very involved problem that I've
moved on from for now - but I will go back to it! (I am convinced it will as usual run for far too long though,
when I finally get the whole thing working.) The below is mainly an assortment of general-use functions, which
from extensive manual testing seem to do what they're supposed to. I don't yet know which of them I'll keep and
use in the full solution. -}

import qualified Data.Map.Strict as Map
import Data.Function (on)
import Data.Tuple (swap)
import Data.Maybe (fromJust)
import Data.Foldable (minimumBy)
import qualified Data.Set as Set

type Position = (Int, Int)
type Layout = Map.Map Position Char

parseInput :: String -> Layout
-- ugly parse function, similar to the one used for day 13 (but somewhat less complicated)
parseInput input = fst $ foldr verticalFold (Map.empty, (length horizontals) - 1) horizontals
    where horizontals = lines input
          parseLine str lineNo = fst $ foldr (horizontalFold lineNo) (Map.empty, (length str) - 1) str
          horizontalFold lineNo chr (lineMap, idx) = (Map.insert (idx, lineNo) chr lineMap, idx - 1)
          verticalFold line (finalMap, lineNo) = (Map.union finalMap (parseLine line lineNo), lineNo - 1)

getData :: IO Layout
getData = readFile "input15.txt" >>= (return . parseInput)

adjacent :: Layout -> Position -> Set.Set Position
-- gets a list of all positions which are adjacent to the present one, which are in the grid
-- and not occupied by a wall (#)
adjacent layout (x, y) = Set.fromList [(z, w) | z <- [0..31], w <- [0..31],
                                        Map.lookup (z, w) layout `elem` [Just '.', Just 'G', Just 'E'],
                                        (z == x && abs (w - y) == 1) || (w == y && abs (z - x) == 1)]

reachableIn :: Layout -> Position -> Int -> Set.Set Position
-- given the layout, a starting point, and a max distance, determines all points (in the grid, not including walls)
-- which can be reached in (exactly) that many steps
reachableIn grid pos 1 = adjacent grid pos
reachableIn grid pos n = Set.fromList $ 
                            (filter (\sq -> Map.lookup sq grid == Just '.')
                            $ Set.toList $ reachableIn grid pos (n - 1))
                            >>= (Set.toList . (adjacent grid))

withinDistance :: Layout -> Position -> Int -> Set.Set Position
-- the same idea as the above, but now including all which can be reached in the given number of steps
-- OR FEWER
withinDistance grid pos range = Set.unions $ map (reachableIn grid pos) [1..range]
 
minDistance :: Layout -> Position -> Position -> Maybe Int
-- finds the minimum distance from one square to another, walking over only empty spaces (but allowing the
-- destination to be occupied by a unit). Yields Nothing if it is not possible to walk from one to the other
minDistance grid start end = check 1
    where check n = if end `Set.member` reachableIn grid start n
                        then Just n
                        else if (withinDistance grid start n) `Set.isSubsetOf` (withinDistance grid start (n-1))
                            then Nothing
                            else check $ n + 1

adjacentToEnemy :: Layout -> Position -> Set.Set Position
-- given the grid position, and the location of a unit, finds all empty locations which are adjacent to an enemy
adjacentToEnemy grid start = case Map.lookup start grid of
    Just 'G' -> let enemies = Map.keys $ Map.filter (== 'E') grid
                    adjacents = Set.unions $ map (adjacent grid) enemies
                in Set.filter (\pos -> Map.lookup pos grid == Just '.') adjacents
    Just 'E' -> let enemies = Map.keys $ Map.filter (== 'G') grid
                    adjacents = Set.unions $ map (adjacent grid) enemies
                in Set.filter (\pos -> Map.lookup pos grid == Just '.') adjacents
    _ -> Set.empty

nearestTarget :: Layout -> Position -> Maybe Position
-- given the grid and the position of a unit on it, finds, from those squares adjacent to an enemy, which
-- ones are nearest. (Or Nothing if no such position can be reached.)
-- Can't just order the results of adjacentToEnemy by the output of minDistance, because computing minDistance
-- over the whole set would be *incredibly* slow. So instead we walk through all spaces 1 away, 2 away etc,
-- until we find a spot adjacent to an enemy.
nearestTarget grid pos = check 1
    where check n = let possibles = Set.intersection (adjacentToEnemy grid pos) (withinDistance grid pos n)
                    in if Set.null possibles
                        then if (withinDistance grid pos n) `Set.isSubsetOf` (withinDistance grid pos (n-1))
                            then Nothing
                            else check (n+1)
                        else Just $ minimumBy (compare `on` swap) $ possibles

shortestPaths :: Layout -> Position -> Position -> Maybe [[Position]]
-- finds the shortest paths between the two positions, as a list of the locations gone over on the way.
-- Returns Nothing if there is no path, otherwise Just a list of the shortest ones
shortestPaths grid start end = check 1 [[start]]
    where check n paths = if (withinDistance grid start n) `Set.isSubsetOf` (withinDistance grid start (n-1))
                            then Nothing
                            else let extended = paths >>=
                                        (\path@(x:xs) -> [(next:path) | next <- Set.toList (adjacent grid x),
                                                                         Map.lookup next grid == Just '.',
                                                                         not $ next `elem` path])
                                     rightPaths = filter ((== end) . head) extended
                                 in if null rightPaths
                                        then check (n+1) extended
                                        else Just rightPaths

nextMove :: Layout -> Position -> Maybe Position
-- for a unit in the grid, find the next position it should move to (if any), according to the rules in the puzzle
-- Once again, we have to be careful not to need to calculate all the paths to all the target squares,
-- but instead gradually move up in distance until we find something
nextMove grid unit = check 1
    where check n = if (withinDistance grid unit n) `Set.isSubsetOf` (withinDistance grid unit (n-1))
                        then Nothing
                        else let reachable = (reachableIn grid unit n) `Set.intersection`
                                                (adjacentToEnemy grid unit)
                             in if Set.null reachable
                                    then check (n+1)
                                    else Just $ minimumBy (compare `on` swap) $ Set.foldr (++) [] $ Set.map
                                            ((map (\path -> (reverse path)!!1)) . fromJust)
                                            $ Set.filter (/= Nothing) $ Set.map (shortestPaths grid unit) reachable
