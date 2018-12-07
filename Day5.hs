import Data.Char (toUpper, isUpper)
import Data.Function (on)

getData :: IO String
getData = readFile "input5.txt"

reactChar :: Char -> Char -> String
-- tests 2 chars against each other to see if they "react" according to the rules of the puzzle
-- if the letters are the same but differ in case, they "cancel out" leaving the empty string
-- otherwise they simply append
reactChar first second =
    let sameCase = (==) `on` isUpper
        sameLetter = (==) `on` toUpper
        pairReacts first second = sameLetter first second && (not $ sameCase first second)
    in if pairReacts first second then "" else [first, second]

reactFully :: Char -> String -> String
-- takes a string and a char to be put at the start of it, following any reactions all the way through
reactFully char [] = [char]
reactFully char (x:xs) = let firstResult = reactChar char x
                                in if length firstResult == 2
                                    then firstResult ++ xs
                                    else if null xs
                                            then []
                                            else reactFully (head xs) (tail xs)

solveFirst :: String -> Int
solveFirst = length . (foldr reactFully "")

first = do
    input <- getData
    putStrLn $ show $ solveFirst input

remove :: Char -> String -> String
-- simply removes all instances (upper and lower) of the given char from the given string
remove char = filter (((/=) `on` toUpper) char)

resultsOfRemovals :: String -> [Int]
-- a list of all 26 results given by removing a single letter and running the reaction again
resultsOfRemovals str = map (\char -> solveFirst $ remove char str) ['a'..'z']

solveSecond :: String -> Int
solveSecond = minimum . resultsOfRemovals

second = do
    input <- getData
    putStrLn $ show $ solveSecond input
