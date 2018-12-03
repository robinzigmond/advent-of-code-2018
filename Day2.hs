getData :: IO String
getData = readFile "input2.txt"

getStrings :: IO [String]
getStrings = do
    input <- getData
    return $ lines input

elemCount :: (Eq a) => a -> [a] -> Int
-- counts how many times a given element occurs in a list
elemCount x = foldr (\y acc -> if y == x then acc + 1 else acc) 0

hasExactly :: (Eq a) => Int -> [a] -> Bool
-- determines if any character occurs exactly a given number of times in the given list
hasExactly n xs = foldr (\x acc -> if elemCount x xs == n then True else acc) False xs

solveFirst :: [String] -> Int
solveFirst xs = (length $ filter (hasExactly 2) xs) * (length $ filter (hasExactly 3) xs)

first = do
    input <- getStrings
    putStrLn $ show $ solveFirst input

getPairs :: (Eq a) => [a] -> [(a,a)]
-- simple utility function to get all pairs of distinct elements in a list
getPairs xs = [(x,y) | x <- xs, y <- xs, x /= y]

exactlyOneDiff :: (Eq a) => ([a], [a]) -> Bool
-- checks if a pair of lists has *exactly one* element which is different in the same position
exactlyOneDiff (xs, ys)
    | length xs /= length ys = False
    | otherwise = numDiffs == 1
    where numDiffs = foldr foldFunc 0 [0..(length xs - 1)]
          foldFunc n acc = if xs!!n == ys!!n then acc else acc + 1

getCorrectPair :: (Eq a) => [([a], [a])] -> Maybe ([a], [a])
-- tests each pair in the list using the above function, and returns the first pair that matches (if it exists)
getCorrectPair = foldr foldFunc Nothing
    where foldFunc _ (Just pair) = Just pair
          foldFunc (x, y) Nothing = if exactlyOneDiff (x, y) then Just (x, y) else Nothing

getSameLetters :: (Eq a) => Maybe ([a], [a]) -> [a]
-- function to take a pair which is known to have exactly one difference, and get the list of common elements
getSameLetters Nothing = []
getSameLetters (Just (xs, ys)) = foldr foldFunc [] [0..(length xs - 1)]
    where foldFunc n acc
            | xs!!n == ys!!n = xs!!n : acc
            | otherwise = acc

solveSecond :: [String] -> String
solveSecond = getSameLetters . getCorrectPair . getPairs

second = do
    input <- getStrings
    putStrLn $ show $ solveSecond input
