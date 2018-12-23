data Region = Rocky | Narrow | Wet deriving (Eq, Show)
type Location = (Int, Int)

depth :: IO Int
depth = (readFile "input22.txt") >>= (return . read . last . words . (head . lines))

target :: IO Location
target = (readFile "input22.txt") >>= \input -> return $ read $ "(" ++ (last . words . (last . lines)) input ++ ")"

geologicIndex :: Location -> Int -> Location -> Int
-- computes the geologic index of a location in the cave, based on the target location and cave depth
geologicIndex _ _ (0, 0) = 0
geologicIndex tgt _ loc
    | loc == tgt = 0
geologicIndex _ _ (x, 0) = x*16807
geologicIndex _ _ (0, y) = y*48271
geologicIndex tgt depth (x, y) = (erosionLevel tgt depth (x-1, y)) * (erosionLevel tgt depth (x, y-1))

erosionLevel :: Location -> Int -> Location -> Int
erosionLevel tgt depth loc = (geologicIndex tgt depth loc + depth) `mod` 20183

regionType :: Location -> Int -> Location -> Region
regionType tgt depth loc = convert $ (\x -> x `mod` 3) $ erosionLevel tgt depth loc
    where convert 0 = Rocky
          convert 1 = Wet
          convert 2 = Narrow

riskLevel :: Location -> Int -> Location -> Int
-- version of the above without converting to the region type. Will simplify calculations.
riskLevel tgt depth loc = (\x -> x `mod` 3) $ erosionLevel tgt depth loc

solveFirst :: Location -> Int -> Int
-- compute the total risk level in the rectange with (0,0) and the target as corners. Runs FAR too slow though,
-- need a cleverer way to compute this in order to get the solution.
solveFirst tgt depth = sum $ (map (riskLevel tgt depth)) locations
    where locations = [(x, y) | x <- [0..(fst tgt)], y <- [0..(snd tgt)]]

first :: IO ()
first = do
    dpth <- depth
    tgt <- target
    putStrLn $ show $ solveFirst tgt dpth
