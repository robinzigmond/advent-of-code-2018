-- copied from puzzle 19, almost entirely!
-- I did solve this (part 1) "on paper", but despite several recalculations my answer wasn't accepted. I know though that
-- the "program" shouldn't run for that long, before I reach the crucial point, which is where I need to read off
-- a value from register 5. So the investigateFirst function below allows me to see what is going on - the value
-- of the first register actually given doesn't matter (it never changes, just serves as a flag to signal the
-- control flow)

import Data.Bits ((.&.), (.|.))

type Register = Int
type Registers = [Register]
-- now need to keep track of the current value of the instruction pointer
type PuzzleState = (Int, Registers)

type OpcodeFunc = Registers -> Int -> Int -> Int -> Registers
type Instruction = (OpcodeFunc, Int, Int, Int)

instructionPointer = 1

initialState :: PuzzleState
initialState = (0, [0, 0, 0, 0, 0, 0])

-- repeat definitions of the 16 functions from the previous puzzle. Just adapt now to having 6 registers
-- instead of 4

addr :: OpcodeFunc
addr reg@[u, v, w, x, y, z] a b c = case c of
    0 -> [(reg!!a) + (reg!!b), v, w, x, y, z]
    1 -> [u, (reg!!a) + (reg!!b), w, x, y, z]
    2 -> [u, v, (reg!!a) + (reg!!b), x, y, z]
    3 -> [u, v, w, (reg!!a) + (reg!!b), y, z]
    4 -> [u, v, w, x, (reg!!a) + (reg!!b), z]
    5 -> [u, v, w, x, y, (reg!!a) + (reg!!b)]

addi :: OpcodeFunc
addi reg@[u, v, w, x, y, z] a b c = case c of
    0 -> [(reg!!a) + b, v, w, x, y, z]
    1 -> [u, (reg!!a) + b, w, x, y, z]
    2 -> [u, v, (reg!!a) + b, x, y, z]
    3 -> [u, v, w, (reg!!a) + b, y, z]
    4 -> [u, v, w, x, (reg!!a) + b, z]
    5 -> [u, v, w, x, y, (reg!!a) + b]

mulr :: OpcodeFunc
mulr reg@[u, v, w, x, y, z] a b c = case c of
    0 -> [(reg!!a) * (reg!!b), v, w, x, y, z]
    1 -> [u, (reg!!a) * (reg!!b), w, x, y, z]
    2 -> [u, v, (reg!!a) * (reg!!b), x, y, z]
    3 -> [u, v, w, (reg!!a) * (reg!!b), y, z]
    4 -> [u, v, w, x, (reg!!a) * (reg!!b), z]
    5 -> [u, v, w, x, y, (reg!!a) * (reg!!b)]

muli :: OpcodeFunc
muli reg@[u, v, w, x, y, z] a b c = case c of
    0 -> [(reg!!a) * b, v, w, x, y, z]
    1 -> [u, (reg!!a) * b, w, x, y, z]
    2 -> [u, v, (reg!!a) * b, x, y, z]
    3 -> [u, v, w, (reg!!a) * b, y, z]
    4 -> [u, v, w, x, (reg!!a) * b, z]
    5 -> [u, v, w, x, y, (reg!!a) * b]

banr :: OpcodeFunc
banr reg@[u, v, w, x, y, z] a b c = case c of
    0 -> [(reg!!a) .&. (reg!!b), v, w, x, y, z]
    1 -> [u, (reg!!a) .&. (reg!!b), w, x, y, z]
    2 -> [u, v, (reg!!a) .&. (reg!!b), x, y, z]
    3 -> [u, v, w, (reg!!a) .&. (reg!!b), y, z]
    4 -> [u, v, w, x, (reg!!a) .&. (reg!!b), z]
    5 -> [u, v, w, x, y, (reg!!a) .&. (reg!!b)]

bani :: OpcodeFunc
bani reg@[u, v, w, x, y, z] a b c = case c of
    0 -> [(reg!!a) .&. b, v, w, x, y, z]
    1 -> [u, (reg!!a) .&. b, w, x, y, z]
    2 -> [u, v, (reg!!a) .&. b, x, y, z]
    3 -> [u, v, w, (reg!!a) .&. b, y, z]
    4 -> [u, v, w, x, (reg!!a) .&. b, z]
    5 -> [u, v, w, x, y, (reg!!a) .&. b]

borr :: OpcodeFunc
borr reg@[u, v, w, x, y, z] a b c = case c of
    0 -> [(reg!!a) .|. (reg!!b), v, w, x, y, z]
    1 -> [u, (reg!!a) .|. (reg!!b), w, x, y, z]
    2 -> [u, v, (reg!!a) .|. (reg!!b), x, y, z]
    3 -> [u, v, w, (reg!!a) .|. (reg!!b), y, z]
    4 -> [u, v, w, x, (reg!!a) .|. (reg!!b), z]
    5 -> [u, v, w, x, y, (reg!!a) .|. (reg!!b)]

bori :: OpcodeFunc
bori reg@[u, v, w, x, y, z] a b c = case c of
    0 -> [(reg!!a) .|. b, v, w, x, y, z]
    1 -> [u, (reg!!a) .|. b, w, x, y, z]
    2 -> [u, v, (reg!!a) .|. b, x, y, z]
    3 -> [u, v, w, (reg!!a) .|. b, y, z]
    4 -> [u, v, w, x, (reg!!a) .|. b, z]
    5 -> [u, v, w, x, y, (reg!!a) .|. b]

setr :: OpcodeFunc
setr reg@[u, v, w, x, y, z] a _ c = case c of
    0 -> [reg!!a, v, w, x, y, z]
    1 -> [u, reg!!a, w, x, y, z]
    2 -> [u, v, reg!!a, x, y, z]
    3 -> [u, v, w, reg!!a, y, z]
    4 -> [u, v, w, x, reg!!a, z]
    5 -> [u, v, w, x, y, reg!!a]

seti :: OpcodeFunc
seti reg@[u, v, w, x, y, z] a _ c = case c of
    0 -> [a, v, w, x, y, z]
    1 -> [u, a, w, x, y, z]
    2 -> [u, v, a, x, y, z]
    3 -> [u, v, w, a, y, z]
    4 -> [u, v, w, x, a, z]
    5 -> [u, v, w, x, y, a]

gtir :: OpcodeFunc
gtir reg@[u, v, w, x, y, z] a b c = let bit = if a > (reg!!b) then 1 else 0
    in case c of
        0 -> [bit, v, w, x, y, z]
        1 -> [u, bit, w, x, y, z]
        2 -> [u, v, bit, x, y, z]
        3 -> [u, v, w, bit, y, z]
        4 -> [u, v, w, x, bit, z]
        5 -> [u, v, w, x, y, bit]

gtri :: OpcodeFunc
gtri reg@[u, v, w, x, y, z] a b c = let bit = if (reg!!a) > b then 1 else 0
    in case c of
        0 -> [bit, v, w, x, y, z]
        1 -> [u, bit, w, x, y, z]
        2 -> [u, v, bit, x, y, z]
        3 -> [u, v, w, bit, y, z]
        4 -> [u, v, w, x, bit, z]
        5 -> [u, v, w, x, y, bit]

gtrr :: OpcodeFunc
gtrr reg@[u, v, w, x, y, z] a b c = let bit = if (reg!!a) > (reg!!b) then 1 else 0
    in case c of
        0 -> [bit, v, w, x, y, z]
        1 -> [u, bit, w, x, y, z]
        2 -> [u, v, bit, x, y, z]
        3 -> [u, v, w, bit, y, z]
        4 -> [u, v, w, x, bit, z]
        5 -> [u, v, w, x, y, bit]

eqir :: OpcodeFunc
eqir reg@[u, v, w, x, y, z] a b c = let bit = if a == (reg!!b) then 1 else 0
    in case c of
        0 -> [bit, v, w, x, y, z]
        1 -> [u, bit, w, x, y, z]
        2 -> [u, v, bit, x, y, z]
        3 -> [u, v, w, bit, y, z]
        4 -> [u, v, w, x, bit, z]
        5 -> [u, v, w, x, y, bit]

eqri :: OpcodeFunc
eqri reg@[u, v, w, x, y, z] a b c = let bit = if (reg!!a) == b then 1 else 0
    in case c of
        0 -> [bit, v, w, x, y, z]
        1 -> [u, bit, w, x, y, z]
        2 -> [u, v, bit, x, y, z]
        3 -> [u, v, w, bit, y, z]
        4 -> [u, v, w, x, bit, z]
        5 -> [u, v, w, x, y, bit]

eqrr :: OpcodeFunc
eqrr reg@[u, v, w, x, y, z] a b c = let bit = if (reg!!a) == (reg!!b) then 1 else 0
    in case c of
        0 -> [bit, v, w, x, y, z]
        1 -> [u, bit, w, x, y, z]
        2 -> [u, v, bit, x, y, z]
        3 -> [u, v, w, bit, y, z]
        4 -> [u, v, w, x, bit, z]
        5 -> [u, v, w, x, y, bit]

-- function for updating the state based on an instruction
doInstruction :: Instruction -> PuzzleState -> PuzzleState
doInstruction inst = (incrementPointer inst) . (writeToPointer inst) . (performOpcode inst) . (writePointer inst)
    where writePointer _ (ip, reg) = let updated = map (\idx -> if idx == instructionPointer then ip else reg!!idx)
                                                        [0..5]
                                     in (ip, updated)
          performOpcode (f, x, y, z) (ip, reg) = (ip, f reg x y z)
          writeToPointer _ (ip, reg) = (reg!!instructionPointer, reg)
          incrementPointer _ (ip, reg) = (ip + 1, reg)

fullyCompute :: [Instruction] -> PuzzleState -> PuzzleState
-- given an instruction set, keeps doing "the process" until the pointer falls outside of the range
fullyCompute instrs (ip, reg) = if (ip < 0) || (ip >= (fromIntegral $ length instrs))
                                    then (ip, reg)
                                    else fullyCompute instrs $ doInstruction (instrs!!ip) (ip, reg)

-- functions to get data from file
parseInstr :: String -> Instruction
parseInstr str = (opcode, read (parts!!1), read (parts!!2), read (parts!!3))
    where parts = words str
          opcode = case (head parts) of
                    "addr" -> addr
                    "addi" -> addi
                    "mulr" -> mulr
                    "muli" -> muli
                    "banr" -> banr
                    "bani" -> bani
                    "borr" -> borr
                    "bori" -> bori
                    "setr" -> setr
                    "seti" -> seti
                    "gtir" -> gtir
                    "gtri" -> gtri
                    "gtrr" -> gtrr
                    "eqir" -> eqir
                    "eqri" -> eqri
                    "eqrr" -> eqrr

getData :: IO [Instruction]
getData = (readFile "input21.txt") >>= (return . (map parseInstr) . tail . lines)

investigateFirst :: IO ()
investigateFirst = getData >>= (\instrs -> doIt instrs 1 (0, [0, 0, 0, 0, 0, 0]))
    where doIt instrs n state@(ip, regs) = do
            putStrLn $ "iteration " ++ show n
            putStrLn $ "doing instruction with index " ++ show ip
            let newState = doInstruction (instrs!!ip) state
            putStrLn $ show newState
            doIt instrs (n+1) newState

showRelevant :: IO ()
-- it turns out that the 'a's that work are all those that can appear in the final register (5) when instruction
-- #28 is executed. Unfortunately, the algorithm to compute these is not straightforward. Going to just get some
-- displayed and perhaps have a look. It also has the major benefit of showing the solution to the first puzzle,
-- which is simply the first value output for register 4.
showRelevant = getData >>= (\instrs -> doIt instrs 1 (0, [0, 0, 0, 0, 0, 0]))
    where doIt instrs n state@(ip, regs) = do
            if ip == 28
                then do
                        putStrLn $ "relevant value! (Iteration no " ++ show n ++ ")"
                        putStrLn $ show regs
                else return ()
            let nextState = doInstruction (instrs!!ip) state
            doIt instrs (n+1) nextState

valuesToHalt :: [Instruction] -> [Int]
-- computes the list, in order, of those values which appear as register 5 when instruction 28 is about
-- to be executed
valuesToHalt instrs = map ((\reg -> reg!!5). snd) $ filter (\(ip, reg) -> ip == 28) allStates
    where allStates = initialState : (map (\st@(ip, reg) -> doInstruction (instrs!!ip) st) allStates)

showThings :: IO [Int]
showThings = getData >>= (lookForRepeats . valuesToHalt)
    where lookForRepeats [] = error "empty list!"
          lookForRepeats xs = doIt 0 xs
          doIt n xs = if xs!!n `elem` (take n xs)
                        then error "cycle detected"
                        else do
                            putStrLn $ show $ xs!!n
                            doIt (n+1) xs

-- now come the crucial definitions to solve the second part!

-- this type represents the instruction number we are on, and the values of registers 3, 4 and 5
type RelevantState = (Int, (Int, Int, Int))

-- these are the values when we first hit instruction 28 (corresponding to solution of first part)
firstState = (28, (0, 1, 3941014))

transform :: RelevantState -> RelevantState
-- the transformation rules, discermed from careful analysis of the program
transform (ip, (a, b, c)) = case ip of
    6 -> (7, (a, c .|. 65536, c))
    7 -> (8, (a, b, 13159625))
    8 -> (9, (b `mod` 256, b, c))
    9 -> (10, (a, b, a+c))
    10 -> (11, (a, b, c `mod` 16777216))
    11 -> (12, (a, b, c*65899))
    12 -> (13, (a, b, c `mod` 16777216))
    13 -> if b < 256
            then (28, (1, b, c))
            else (24, (0, b, c))
    24 -> (26, (floor ((fromIntegral b) / (fromIntegral 256)), b, c)) 
    26 -> (8, (a, a, c))
    28 -> (6, (0, b, c))

-- note, this is exactly the same list as valusToHalt above (which I made when I was floundering a bit with
-- this). But it has a HUGE practical difference, this simplified (but equivalent) algorithem computes hundreds
-- of values per second, whereas the previous version only did a few per minute! Hence the cycle can be found
-- quickly.
allRelevantStates :: [RelevantState]
allRelevantStates = filter ((==28) . fst) allStates
    where allStates = firstState : (map transform allStates)

figureItOut :: IO ()
figureItOut = lookForRepeats allRelevantStates
    where lookForRepeats [] = error "empty list!"
          lookForRepeats xs = doIt 0 xs
          doIt n xs = do
                        putStrLn $ show $ (\(_, _, x) -> x) $ snd $ xs!!n
                        if (xs!!n `elem` (take n xs))
                            then do
                                putStrLn "cycle starting next!"
                                putStrLn $ "value is " ++ (show $ (\(_, _, x) -> x) $ snd $ xs!!n)
                                return ()
                            else doIt (n+1) xs
