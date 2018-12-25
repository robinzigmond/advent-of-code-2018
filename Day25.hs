type SpaceTimePoint = (Int, Int, Int, Int)

parseLine :: String -> SpaceTimePoint
parseLine str = read $ "(" ++ str ++ ")"

getData :: IO [SpaceTimePoint]
getData = readFile "input25.txt" >>= (return . (map parseLine) . lines)

distance :: SpaceTimePoint -> SpaceTimePoint -> Int
distance (a, b, c, d) (w, x, y, z) = sum $ map abs [a-w, b-x, c-y, d-z]

isInChain :: SpaceTimePoint -> [SpaceTimePoint] -> Bool
isInChain point = (any id) . (map ((<= 3) . (distance point)))

getConstellations :: [SpaceTimePoint] -> [[SpaceTimePoint]]
-- gets the list of constellations from the full list of points
getConstellations = foldr foldFunc []
    where foldFunc point consts
            | null (overlap point consts) = [point]:consts
            | length (overlap point consts) == 1 = map (\cnst -> if cnst `elem` overlap point consts
                                                                    then point:cnst
                                                                    else cnst) consts
            | otherwise = let untouched = filter (not . (isInChain point)) consts
                          in (point : concat (overlap point consts)):untouched
          overlap pt = filter (isInChain pt)

solveFirst :: [SpaceTimePoint] -> Int
solveFirst = length . getConstellations

-- took about 10 minutes, not too bad
first :: IO ()
first = getData >>= (putStrLn . show . solveFirst)
