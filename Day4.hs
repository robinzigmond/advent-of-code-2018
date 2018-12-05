{- I apologise profusely to all genuine Haskell developers for the horrible mess of barely readable code
below. I just couldn't get on with this problem at all, and this has already been through many rewrites
(over and above the usual corrections for type errors and so on) that it's ended up even more of a mess
that it started out as.

Still, ugly though it is (and inefficient too, see comment above the solveSecond function) - it worked,
and that's the main thing! :-)
-}

import Data.List (isInfixOf, nub, sortOn, (\\), findIndex)

getData :: IO [String]
getData = do
    input <- readFile "input4.txt"
    return $ lines input

getDate :: String -> String
-- simply extracts the date (as a string) from one line of the file
-- a very simple function but it's needed in a few different places -
-- and it needs to account for the fact that the time can be before midnight,
-- in which case we count it as belonging to the next day
getDate line = let date = map (\idx -> line!!idx) [1..10]
                   hour = read $ map (\idx -> line!!idx) [12, 13] :: Int
               in if hour > 0 then nextDate date else date

daysInMonths :: [Int]
-- note 1518 was NOT a leap year
daysInMonths = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

nextDate :: String -> String
-- find next date after the given one, with all dates formatted as in the puzzle (yyyy-mm-dd)
-- fortunately we don't have to cope with 31st December (in fact there are no dates in December)
nextDate dateStr = "1518-" ++ pad month ++ "-" ++ pad day
                 where currentMonth = read $ map (\idx -> dateStr!!idx) [5, 6] :: Int
                       currentDay = read $ map (\idx -> dateStr!!idx) [8, 9] :: Int
                       day = if currentDay == daysInMonths!!(currentMonth - 1)
                                then 1
                                else currentDay + 1
                       month = if currentDay == daysInMonths!!(currentMonth - 1)
                                    then currentMonth + 1
                                    else currentMonth
                       pad num = if (length . show) num == 1 then ('0':show num) else show num

getDates :: Int -> [String] -> [String]
-- for a given guard number, and list of input, find (from the input) which dates (if any) they were on duty
-- the date will be given in string form
getDates guardNo input = nub $ map getDate relevant
    where relevant = filter (\str -> ("Guard #" ++ show guardNo) `isInfixOf` str) input

getGuardData :: Int -> [String] -> [String]
-- for a give guard number, and list of input and returns a (possibly empty) list of those lines
-- which features that guard
getGuardData guardNo input = filter (\line -> (getDate line) `elem` dates) input
    where dates = getDates guardNo input

inputForGuardAndDate :: Int -> String -> [String] -> [String]
-- given a guard and a string representing a particular date, plus the full input list,
-- find only those strings that represent his activity during that "day"
inputForGuardAndDate guardNo date input = filter (\info -> getDate info == date) fullData
    where fullData = getGuardData guardNo input

whenAsleep :: [String] -> [Bool]
-- given a list of strings, each representing a line of input from the puzzle (from the same date),
-- outputs an array of length 60, showing each minute whether (or not) the guard was asleep
whenAsleep xs = map mapping [0..59]
    where beforeMidnight = filter (\str -> (read (map (\idx -> str!!idx) [12, 13]) :: Int) > 0) xs
          afterMidnight = xs \\  beforeMidnight
          readMinute str = read $ map (\idx -> str!!idx) [15, 16] :: Int
          orderedBefore = sortOn readMinute beforeMidnight
          orderedAfter = sortOn readMinute afterMidnight
          startState = if null orderedBefore then True else "falls asleep" `isInfixOf` last orderedBefore
          recentUpdate minute = if null orderedAfter -- guard #241, arrives and stays awake!
                                    then ""
                                    else if minute < (readMinute $ head orderedAfter)
                                        then if startState
                                            then "falls asleep"
                                            else ""
                                        else last $ filter (\str -> readMinute str <= minute) orderedAfter 
          mapping minute = "falls asleep" `isInfixOf` recentUpdate minute

timeAsleep :: [Bool] -> Int
-- simple convenience function which simply counts the "true" values in a list of bools
timeAsleep = length . (filter id)

totalTimeAsleep :: Int -> [String] -> Int
-- given a guard number, and list of input, works out the total time they were asleep
-- (from midnight) on all nights they were guarding
totalTimeAsleep guardNo input = sum $ map timeAsleep allSleepLists
    where allSleepLists = map whenAsleep allInfo
          allInfo = map (\dt -> inputForGuardAndDate guardNo dt input) allDates
          allDates = getDates guardNo input

howOftenAsleep :: Int -> Int -> [String] -> Int
-- given a guard number, and a minute (and the input!), how often is the guard asleep at that minute
howOftenAsleep guardNo minute input = length [lst | lst <- allSleepLists, lst!!minute]
    where allSleepLists = map whenAsleep allInfo
          allInfo = map (\dt -> inputForGuardAndDate guardNo dt input) allDates
          allDates = getDates guardNo input

getAllGuards :: [String] -> [Int]
-- get list of all used guard numbers from the input
getAllGuards input = nub $ map getNumber relevant
    where getNumber str = if str!!29 == 'b'
                            then (read $ map (\idx -> str!!idx) [26..28]) :: Int
                            else (read $ map (\idx -> str!!idx) [26..29]) :: Int
                            -- correct index range for guard id (including possible space, but allowing for 2 digits)
                            -- if the # is in the string
          relevant = filter (\str -> "Guard #" `isInfixOf` str) input

mostSleepy :: [String] -> Int
-- find the guard most often asleep, from the given input
mostSleepy input = possGuards!!idx
    where Just idx = findIndex (== maxSleepTotal) sleepTotals
          maxSleepTotal = maximum sleepTotals
          sleepTotals = map (\min -> totalTimeAsleep min input) possGuards
          possGuards = getAllGuards input

solveFirst :: [String] -> Int
solveFirst input = guardNo * idx
    where guardNo = mostSleepy input
          totalSleepEachMin = map (\min -> howOftenAsleep guardNo min input) [0..59]
          maxMin = maximum totalSleepEachMin
          Just idx = findIndex (== maxMin) totalSleepEachMin

first = do
    input <- getData
    putStrLn $ show $ solveFirst input

-- this ran for over 15 minutes before spitting out the (correct) solution
-- really not sure how to make it more efficient
solveSecond :: [String] -> Int
solveSecond input = guardNo * minute
    where possGuards = getAllGuards input
          sleepPerMinPerGuard = map (\guardNo -> map (\min -> howOftenAsleep guardNo min input) [0..59]) possGuards
          maxSleep = maximum $ concat sleepPerMinPerGuard
          correctMinuteData = head $ filter (\times -> maxSleep `elem` times) sleepPerMinPerGuard
          Just idx = findIndex (==correctMinuteData) sleepPerMinPerGuard
          guardNo = possGuards!!idx
          Just minute = findIndex (== maxSleep) correctMinuteData

second = do
    input <- getData
    putStrLn $ show $ solveSecond input
