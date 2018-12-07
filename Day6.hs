import Data.List (nub, sortOn)

type Point = (Int, Int)

parseStr :: String -> Point
-- convert comma-separated pair of integers (as a string) into a Point (pair of ints)
parseStr str = read $ "(" ++ str ++ ")" :: Point

getData :: IO [Point]
getData = do
    input <- readFile "input6.txt"
    return $ map parseStr $ lines input

distance :: Point -> Point -> Int
-- get Manhattan distance between 2 points
distance (x1, y1) (x2, y2) = (abs $ x2 - x1) + (abs $ y2 - y1)

nearest :: Point -> [Point] -> [Point]
-- given a point and a list of points, returns the point(s) in the list which are closest
nearest pnt pnts = let minDist = minimum $ map (\pt -> distance pt pnt) pnts
                        in filter (\pt -> distance pt pnt == minDist) pnts

isFinite :: [Point] -> Point -> Bool
{- given a list of points, and one point from that list, this function determines
if the list of all points (in the infinite plane) which are nearer to that point
than any other in the list is finite or not.
I have used the following algorithm to compute this in finite time:

Find the smallest rectangle (with corner at (0,0)) which contains all points in the list.
For each point on the boundary of this rectangle, compute its nearest point from the list.
All of these "nearest points" to those on the boundary have an infinite "nearest list"
over the whole plane, and no others do.

The truth of this statement is quite easy to see if you stop and consider it. (Although I
haven't come up with a full mathematical proof yet.) -}
isFinite pnts pnt = not $ pnt `elem` boundaryNearest
    where furthestRight = last $ sortOn fst pnts
          furthestDown = last $ sortOn snd pnts
          topEdge = [(x, 0) | x <- [0..fst furthestRight]]
          bottomEdge = [(x, snd furthestDown) | x <- [0..fst furthestRight]]
          leftEdge = [(0, y) | y <- [0..snd furthestDown]]
          rightEdge = [(fst furthestRight, y) | y <- [0..snd furthestDown]]
          boundary = nub $ topEdge ++ bottomEdge ++ leftEdge ++ rightEdge
          nearestPoints pt = nearest pt pnts
          boundaryNearest = nub $ concat $ filter ((==1) . length) $ map nearestPoints boundary

finites :: [Point] -> [Point]
-- using the function above, determine what subset of the points has the property that the set
-- of all points in the plane closer to that point than any other is finite
finites pnts = filter (isFinite pnts) pnts

solveFirst :: [Point] -> Int
solveFirst pnts = maximum $ map (length . nearestSet) $ finites pnts
    where rightLimit = maximum $ map fst pnts
          bottomLimit = maximum $ map snd pnts
          nearestSet pnt = [(x, y) | x <- [0..rightLimit], y <- [0..bottomLimit], nearest (x, y) pnts == [pnt]]

-- ran for nearly 20 minutes, would be nice to be more efficient...
first :: IO ()
first = getData >>= (putStrLn . show . solveFirst)

totalDistanceWithin :: Point -> [Point] -> Int -> Bool
-- works out, in as efficient a way as possible, whether the sum of all distances
-- from the given point to all points in the list is less than the given number
totalDistanceWithin pnt pnts n = snd $ foldr foldFunc (0, True) pnts
    where foldFunc _ (_, False) = (0, False)
          foldFunc pt (total, True)
            | total + distance pt pnt >= n = (0, False)
            | otherwise = (total + distance pt pnt, True)

-- trying to optimise, finding a starting set of points which is relatively small compared to the range
-- required, but still quick to compute. The following is based on some elementary maths:
startingArea :: [Point] -> Int -> [Point]
startingArea pnts n = [(x, y) | x <- [0..xMax], y <- [0..yMax], distance (x, y) (head pnts) < n]
    where numPoints = length pnts
          xMax = floor ((fromIntegral (n + (sum $ map fst pnts))) / (fromIntegral numPoints))
          yMax = floor ((fromIntegral (n + (sum $ map snd pnts))) / (fromIntegral numPoints))

solveSecond :: [Point] -> Int
solveSecond pnts = length $ filter (\pnt -> totalDistanceWithin pnt pnts 10000) (startingArea pnts 10000)

second :: IO ()
second = getData >>= (putStrLn . show . solveSecond)

-- hooray for "aggressive" optimisation, the above (about my 6th attempt, at least) finally ran in reasonable
-- time - in fact, just a couple of minutes.
