import Data.Bits ((.&.), (.|.))
import Data.List (intersect, sortOn)

type Register = Int
type Registers = [Register]

type Before = [Int]
type Opcode = [Int]
type After = [Int]
type Trial = (Before, Opcode, After)

type OpcodeFunc = Registers -> Int -> Int -> Int -> Registers

parseTrials :: String -> [Trial]
-- parse the first part of the input file into a list of trial information
parseTrials input = splitIntoThrees rawLists
    where relevant = take 3340 $ lines input
          parseBefore str = read $ drop 8 str :: Before
          parseCode = (map read) . words :: String -> Opcode
          parseAfter str = read $ drop 8 str :: After
          parseLine idx = case idx `mod` 4 of
                            0 -> parseBefore $ relevant!!idx
                            1 -> parseCode $ relevant!!idx
                            2 -> parseAfter $ relevant!!idx
                            3 -> []
          rawLists = filter (not . null) $ map parseLine [0..3339]
          splitIntoThrees (a:b:c:ds) = (a, b, c):(splitIntoThrees ds)
          splitIntoThrees _ = []

getTrialData :: IO [Trial]
getTrialData = readFile "input16.txt" >>= (return . parseTrials)

-- define Haskell functions for all 16 possible opcodes:

addr :: OpcodeFunc
addr reg@[w, x, y, z] a b c = case c of
    0 -> [(reg!!a) + (reg!!b), x, y, z]
    1 -> [w, (reg!!a) + (reg!!b), y, z]
    2 -> [w, x, (reg!!a) + (reg!!b), z]
    3 -> [w, x, y, (reg!!a) + (reg!!b)]

addi :: OpcodeFunc
addi reg@[w, x, y, z] a b c = case c of
    0 -> [(reg!!a) + b, x, y, z]
    1 -> [w, (reg!!a) + b, y, z]
    2 -> [w, x, (reg!!a) + b, z]
    3 -> [w, x, y, (reg!!a) + b]

mulr :: OpcodeFunc
mulr reg@[w, x, y, z] a b c = case c of
    0 -> [(reg!!a) * (reg!!b), x, y, z]
    1 -> [w, (reg!!a) * (reg!!b), y, z]
    2 -> [w, x, (reg!!a) * (reg!!b), z]
    3 -> [w, x, y, (reg!!a) * (reg!!b)]

muli :: OpcodeFunc
muli reg@[w, x, y, z] a b c = case c of
    0 -> [(reg!!a) * b, x, y, z]
    1 -> [w, (reg!!a) * b, y, z]
    2 -> [w, x, (reg!!a) * b, z]
    3 -> [w, x, y, (reg!!a) * b]

banr :: OpcodeFunc
banr reg@[w, x, y, z] a b c = case c of
    0 -> [(reg!!a) .&. (reg!!b), x, y, z]
    1 -> [w, (reg!!a) .&. (reg!!b), y, z]
    2 -> [w, x, (reg!!a) .&. (reg!!b), z]
    3 -> [w, x, y, (reg!!a) .&. (reg!!b)]

bani :: OpcodeFunc
bani reg@[w, x, y, z] a b c = case c of
    0 -> [(reg!!a) .&. b, x, y, z]
    1 -> [w, (reg!!a) .&. b, y, z]
    2 -> [w, x, (reg!!a) .&. b, z]
    3 -> [w, x, y, (reg!!a) .&. b]

borr :: OpcodeFunc
borr reg@[w, x, y, z] a b c = case c of
    0 -> [(reg!!a) .|. (reg!!b), x, y, z]
    1 -> [w, (reg!!a) .|. (reg!!b), y, z]
    2 -> [w, x, (reg!!a) .|. (reg!!b), z]
    3 -> [w, x, y, (reg!!a) .|. (reg!!b)]

bori :: OpcodeFunc
bori reg@[w, x, y, z] a b c = case c of
    0 -> [(reg!!a) .|. b, x, y, z]
    1 -> [w, (reg!!a) .|. b, y, z]
    2 -> [w, x, (reg!!a) .|. b, z]
    3 -> [w, x, y, (reg!!a) .|. b]

setr :: OpcodeFunc
setr reg@[w, x, y, z] a _ c = case c of
    0 -> [reg!!a, x, y, z]
    1 -> [w, reg!!a, y, z]
    2 -> [w, x, reg!!a, z]
    3 -> [w, x, y, reg!!a]

seti :: OpcodeFunc
seti [w, x, y, z] a _ c = case c of
    0 -> [a, x, y, z]
    1 -> [w, a, y, z]
    2 -> [w, x, a, z]
    3 -> [w, x, y, a]

gtir :: OpcodeFunc
gtir reg@[w, x, y, z] a b c = let bit = if a > (reg!!b) then 1 else 0
    in case c of
        0 -> [bit, x, y, z]
        1 -> [w, bit, y, z]
        2 -> [w, x, bit, z]
        3 -> [w, x, y, bit]

gtri :: OpcodeFunc
gtri reg@[w, x, y, z] a b c = let bit = if (reg!!a) > b then 1 else 0
    in case c of
        0 -> [bit, x, y, z]
        1 -> [w, bit, y, z]
        2 -> [w, x, bit, z]
        3 -> [w, x, y, bit]

gtrr :: OpcodeFunc
gtrr reg@[w, x, y, z] a b c = let bit = if (reg!!a) > (reg!!b) then 1 else 0
    in case c of
        0 -> [bit, x, y, z]
        1 -> [w, bit, y, z]
        2 -> [w, x, bit, z]
        3 -> [w, x, y, bit]

eqir :: OpcodeFunc
eqir reg@[w, x, y, z] a b c = let bit = if a == (reg!!b) then 1 else 0
    in case c of
        0 -> [bit, x, y, z]
        1 -> [w, bit, y, z]
        2 -> [w, x, bit, z]
        3 -> [w, x, y, bit]

eqri :: OpcodeFunc
eqri reg@[w, x, y, z] a b c = let bit = if (reg!!a) == b then 1 else 0
    in case c of
        0 -> [bit, x, y, z]
        1 -> [w, bit, y, z]
        2 -> [w, x, bit, z]
        3 -> [w, x, y, bit]

eqrr :: OpcodeFunc
eqrr reg@[w, x, y, z] a b c = let bit = if (reg!!a) == (reg!!b) then 1 else 0
    in case c of
        0 -> [bit, x, y, z]
        1 -> [w, bit, y, z]
        2 -> [w, x, bit, z]
        3 -> [w, x, y, bit]

-- now the function to check if a given trial behaves like one of the above opcodes
valid :: OpcodeFunc -> Trial -> Bool
valid func (before, [_, x, y, z], after) = (func before x y z) == after

allOpcodes :: [OpcodeFunc]
allOpcodes = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]

numberValid :: Trial -> Int
-- simple helper to find, for a given trial, the number of opcodes which check out for it
numberValid trial = length $ filter id $ (map valid allOpcodes) <*> [trial]

solveFirst :: [Trial] -> Int
solveFirst = length . (filter (>= 3)) . (map numberValid)

first :: IO ()
first = getTrialData >>= (putStrLn . show . solveFirst)

outcome :: Trial -> (Int, [Int])
-- find specific information about an individual trial - which code it used, and which functions it could
-- have been (coded by their indices in the above list)
outcome trial@(_, [code, _, _, _], _) = (code, filter (\idx -> valid (allOpcodes!!idx) trial) [0..15])

outcomes :: [Trial] -> [(Int, [Int])]
-- collates all the outcomes, to a list of pairs of the 16 opcodes, and the functions (coded by indices)
-- which they could correspond to. Expecting a unique solution!
outcomes = (sortOn fst) . (foldr foldFunc [])
    where foldFunc trial lst = let code = fst $ outcome trial
                                   existing = filter ((== code) . fst) lst
                               in if null existing
                                    then (outcome trial):lst
                                    else let newInfo = snd $ outcome trial
                                             oldInfo = snd $ head existing
                                             totalInfo = newInfo `intersect` oldInfo
                                             keepOrReplace (num, info) = if num == code
                                                                            then (num, totalInfo)
                                                                            else (num, info)
                                         in map keepOrReplace lst

investigate :: IO ()
investigate = getTrialData >>= (putStrLn . show . outcomes)

{- This yields the following output - which I have inserted linebreaks into for nicer formatting:

[(0,[10,11,12,13,14,15]),
(1,[0,1,2,3,4,5,6,7,8,9,10,11,12]),
(2,[4,5,9,10,11,12,13,14,15]),
(3,[11,12,13,14]),
(4,[4,5,6,7,8,9]),
(5,[0,5,6,8,9,10,12]),
(6,[4,10,11,12,13,14,15]),
(7,[14,15]),
(8,[4,5,10,11,12,13,14,15]),
(9,[0,9]),
(10,[0,1,6,7,8,9,10,11,12]),
(11,[15]),
(12,[11,13,15]),
(13,[13,14,15]),
(14,[8,10]),
(15,[0,3,5,6,7,8,12,14])]

from this it is a simple logic puzzle (simpler "on paper" than trying to code it!) to deduce that:

0 is 10 (gtir)
1 is 2 (mulr)
2 is 9 (seti)
3 is 12 (gtrr)
4 is 7 (bori)
5 is 6 (borr)
6 is 4 (banr)
7 is 14 (eqri)
8 is 5 (bani)
9 is 0 (addr)
10 is 1 (addi)
11 is 15 (eqrr)
12 is 11 (gtri)
13 is 13 (eqir)
14 is 8 (setr)
15 is 3 (muli)
-}

solved :: [OpcodeFunc]
solved = [gtir, mulr, seti, gtrr, bori, borr, banr, eqri, bani, addr, addi, eqrr, gtri, eqir, setr, muli]

getProgram :: IO [Opcode]
getProgram = do
    input <- readFile "input16.txt"
    let programText = drop 3342 $ lines input
    return $ map ((map read) . words) programText

solveSecond :: [Opcode] -> Register
solveSecond = head . (foldl (\regs [code, a, b, c] -> (solved!!code) regs a b c) [0, 0, 0, 0])

second :: IO ()
second = getProgram >>= (putStrLn . show . solveSecond)
