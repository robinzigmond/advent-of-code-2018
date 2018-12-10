data Point = Point {x :: Int,  y :: Int} deriving (Eq, Show)
data Velocity = Velocity {vx :: Int,  vy :: Int} deriving Show

parseStr :: String -> (Point, Velocity)
-- read a line of the input to get pairs for the point and velocity
parseStr str = (Point x1 y1, Velocity x2 y2)
            where x1 = read $ map (\idx -> str!!idx) [10..15]
                  y1 = read $ map (\idx -> str!!idx) [18..23]
                  x2 = read $ map (\idx -> str!!idx) [36, 37]
                  y2 = read $ map (\idx -> str!!idx) [40, 41]

getData :: IO [(Point, Velocity)]
getData = readFile "input10.txt" >>= (return . (map parseStr) . lines)

movePoint :: (Point, Velocity) -> (Point, Velocity)
-- moves a point by the given velocity vector, to find its next position. (We keep the velocity in place
-- to allow easier chaining in the main function.)
movePoint (pt, velocity) = (Point (x pt + vx velocity) (y pt + vy velocity), velocity)

xrange :: [Point] -> [Int]
-- simple helper function that we'll need in the main function
xrange pts = [(minimum xs)..(maximum xs)]
            where xs = map x pts

yrange :: [Point] -> [Int]
-- same for the y co-ords
yrange pts = [(minimum ys)..(maximum ys)]
            where ys = map y pts

getDisplay :: [Point] -> [String]
-- format a set of points into a list of strings, ready to display on the terminal
getDisplay pnts = map getHorizontal vrange
                where vrange = yrange pnts
                      hrange = xrange pnts
                      getSymbol x y = if Point {x=x, y=y} `elem` pnts then '#' else '.'
                      getHorizontal ycoord = map (\x -> getSymbol x ycoord) hrange

printMessages :: Int -> Int -> Int -> IO ()
-- main function to display on the terminal the result of each step in the sequence, for a given first
-- and last step. The third parameter is a maximum width to stop anything being displayed if the
-- horizontal range is huge (as it will be at the start)

-- as it happens, the answer appeared at iteration 10334. Which happened to be the correct answer for
-- the second part. (Except needed to subtract 1 because this figure counts starting from 1, whereas 
-- the puzzle starts from 0.)
printMessages first last maxWidth = do
    points <- getData
    doPrint 1 last maxWidth points
    where doPrint current last maxWidth points = do
            if current <= last
                then do
                    if current < first
                        then do
                            -- don't bother showing anything unless we're in the specified range
                            let newPoints = map movePoint points
                            doPrint (current + 1) last maxWidth newPoints
                        else do
                            let width = length (xrange $ map fst points)
                            putStrLn $ show width
                            if width < maxWidth
                                then do
                                        putStrLn $ "narrow enough at iteration " ++ show current
                                        sequence $ map putStrLn $ getDisplay $ map fst points
                                        putStrLn ""
                                        let newPoints = map movePoint points
                                        doPrint (current + 1) last maxWidth newPoints
                                else do
                                        let newPoints = map movePoint points
                                        doPrint (current + 1) last maxWidth newPoints
                else putStrLn "finished"
