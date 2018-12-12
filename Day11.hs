import Data.List (sortOn, findIndex)

type Cell = (Int, Int)
type Grid = [Cell]

powerLevel :: Int -> Cell -> Int
-- compute the power level of a cell, given a particular grid serial number
powerLevel serial (x, y) = let rackID = x + 10
                               numString = show $ (rackID * y + serial) * rackID
                               digit = if length numString < 3 then '0' else numString!!(length numString - 3)
                           in read [digit] - 5

powerOf3x3 :: Int -> Cell -> Int
-- compute the total power of a 3x3 grid, given the co-ords of its top-left corner
powerOf3x3 serial (x, y) = sum $ map (powerLevel serial) [(x', y') | x' <- [x, x+1, x+2], y' <- [y, y+1, y+2]]

first :: Int -> Cell
-- solve first problem by finding the co-ords of the minimum 3x3. Takes serial number as input
first serial = head $ sortOn ((*(-1)) . powerOf3x3 serial) [(x, y) | x <- [1..298], y <- [1..298]]

{-powerOfGrid :: Int -> Int -> Cell -> Int
-- generalisation of the powerOf3x3 function above to allow grids of arbitrary size. The size is given
-- by the second parameter
powerOfGrid serial size (x, y) = sum $ map (powerLevel serial) [(x', y') | x' <- [x..(x+size)-1], y' <- [y..(y+size-1)]]

second :: Int -> (Int, Cell)
second serial = head $ sortOn ((*(-1)) . uncurry (powerOfGrid serial)) 
    $ [(n, (x, y)) | n <- [1..300], x <- [1..(300-n)], y <- [1..(300-n)]]-}

powerOfLShape :: Int -> Int -> Cell -> Int
-- finds the total power of the "L shape" of cells that make up the bottom and right edges
-- of a square of the given size. (Where the size is 1, this is a single cell.) The cell parameter
-- gives the top-left corner of the notional square. This is important to compute the maximum power
-- over squares in an efficient manner
powerOfLShape serial size (x, y) = sum $ map (powerLevel serial) $
                                    [(x + size - 1, y') | y' <- [y..(y + size - 1)]]
                                    ++ [(x', y + size - 1) | x' <- [x..(x + size - 2)]]

biggestSquarePower :: Int -> Cell -> (Cell, Int, Int)
-- given the serial number and a cell, finds from all those squares with that cell as top-left corner,
-- that one with the biggest total power level. Returns that cell, the size, and the found power level
biggestSquarePower serial (x, y) = let totals = tail $ scanl (\total size -> total +
                                        (powerOfLShape serial size (x, y))) 0 [1..(301 - (max x y))]
                                       biggest = maximum totals
                                   in ((x, y), let Just idx = findIndex (== biggest) totals in idx + 1, biggest)

second :: Int -> (Cell, Int)
second serial = let maxesForCells = map (biggestSquarePower serial) [(x, y) | x <- [1..300], y <- [1..300]]
                    (cell, size, max) = head $ sortOn (\(x, y, z) -> -z) maxesForCells
                in (cell, size)
