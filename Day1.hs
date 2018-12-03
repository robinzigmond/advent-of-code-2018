import Data.List

getData :: IO String
getData = readFile "input1.txt"

parseStr :: String -> Int -- assumes input is a numeric string beginning with + or -
parseStr ('+':num) = read num
parseStr ('-':num) = (-1) * (read num)

getNumbers :: IO [Int]
getNumbers = do
    input <- getData
    return $ map parseStr $ lines input

lastMatch :: (Eq a) => [a] -> Bool
-- convenience function to check if the last element of a list has previously occurred in it
lastMatch xs = (last xs) `elem` (init xs)

first = do
    numbers <- getNumbers
    let answer = sum numbers
    putStrLn $ show answer

second = do
    finite <- getNumbers
    let numbers = cycle finite
    let sums = scanl (+) 0 numbers
    let partials = [take n sums | n <- [1..]]
    let results = head $ filter lastMatch partials
    let answer = last results
    putStrLn $ show answer
