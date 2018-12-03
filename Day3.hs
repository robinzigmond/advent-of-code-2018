import Data.Text (pack, unpack, splitOn)
import Data.Set hiding (map, filter, foldr)

getData :: IO String
getData = readFile "input3.txt"

data StartCorner = StartCorner {left :: Int, top :: Int} deriving (Eq, Show, Read)
data Size = Size {width :: Int, height :: Int} deriving (Eq, Show, Read)
type Claim = (Int, StartCorner, Size)

parseLine :: String -> Claim
-- parses a line from the input file into the required data. Doesn't attempt to validate
-- that data has the correct format (because won't need to)
parseLine str = (id, corner, size)
    where parts = words str
          id = read $ tail $ parts!!0 :: Int
          cornerTuple = read $ "(" ++ (init $ parts!!2) ++ ")" :: (Int, Int)
          corner = StartCorner {left=fst cornerTuple, top=snd cornerTuple}
          sizeStr = parts!!3
          dims = map (read . unpack) $ splitOn (pack "x") (pack sizeStr)
          size = Size {width=dims!!0, height=dims!!1}

getInfo :: IO [Claim]
getInfo = do
    input <- getData
    let info = lines input
    return $ map parseLine info

claimedSquares :: Claim -> Set (Int, Int)
claimedSquares (_, StartCorner {left=lft, top=tp}, Size {width=w, height=h})
    = fromList [(x, y) | x <- [lft+1..lft+w], y <- [tp+1..tp+h]]

claimedOnceAndMultiple :: [Claim] -> (Set (Int, Int), Set (Int, Int))
claimedOnceAndMultiple = foldr foldFunc (empty, empty)
    where foldFunc clm (once, mlt) = (newOnce once mlt clm, newMult once mlt clm)
          newMult once mlt clm = (once `intersection` (claimedSquares clm)) `union` mlt
          newOnce once mlt clm = (once `union` (claimedSquares clm)) `difference` newMult once mlt clm

solveFirst :: [Claim] -> Int
solveFirst xs = length $ snd $ claimedOnceAndMultiple xs

first = do
    info <- getInfo
    putStrLn $ show $ solveFirst info

solveSecond :: [Claim] -> Int
solveSecond clms = x
    where singles = fst $ claimedOnceAndMultiple clms
          isolated = filter (\cl -> (claimedSquares cl) `isSubsetOf` singles) clms
          (x, _, _) = isolated!!0 -- assume exactly one isolated claim, as puzzle says

second = do
    info <- getInfo
    putStrLn $ show $ solveSecond info
