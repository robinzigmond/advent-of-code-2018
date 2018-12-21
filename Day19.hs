-- this is obviously a continuation of Day 16, so a lot of the definitions carry over

import Data.Bits ((.&.), (.|.))

type Register = Int
type Registers = [Register]
-- now need to keep track of the current value of the instruction pointer
type PuzzleState = (Int, Registers)

type OpcodeFunc = Registers -> Int -> Int -> Int -> Registers
type Instruction = (OpcodeFunc, Int, Int, Int)

instructionPointer = 4

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
fullyCompute instrs (ip, reg) = if (ip < 0) || (ip >= (length instrs))
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
getData = (readFile "input19.txt") >>= (return . (map parseInstr) . tail . lines)

solveFirst :: [Instruction] -> Register
solveFirst instrs = head $ snd $ fullyCompute instrs initialState

first :: IO ()
first = getData >>= (putStrLn . show . solveFirst)

investigateSecond :: IO ()
-- I know the way these things work, that simple change in the initial value of register 0 is going to make
-- the process take absolutely forever. So instead of simply running the process "blindly" and hoping for a
-- result in reasonable time, this is a script to output all the successive states and hopefully gain a little
-- insight into the process
investigateSecond = getData >>= (\instrs -> doIt instrs 1 (0, [1, 0, 0, 0, 0, 0]))
    where doIt instrs n state@(ip, regs) = do
            putStrLn $ "iteration " ++ show n
            putStrLn $ "doing instruction with index " ++ show ip
            let newState = doInstruction (instrs!!ip) state
            putStrLn $ show newState
            doIt instrs (n+1) newState

{-
The above didn't actually help too much.

However, a careful analysis, by hand, of the various (extremely large!) loops in the instruction set allowed me to
determine the answer. In brief, this is how the algorithm works, through astronomically many steps.
(Warning, spoilers - don't read if you want to figure the puzzle out for yourself.):

- in the preliminary stages, by various operations, register 1 is built up to the huge value of 10551311.
Another significant factor is that register 0 gets reset to 0.
- Shortly after that, two nested loops are entered. The inner loop has little lasting effect other than to
increase register 5 by 1. It can be broken in one of two ways. Either register 2 has to be a factor of 10551311
(and register 5 has to be its co-factor), or register 5 has to reach 10551311 itself. (This latter condition
also meets the former one, the first time it happens, since register 2 happens to hold the value 1 at this
early stage.)
- When the first of these conditions is met, the value of register 2 is added to the register 0, before
resuming the inner loop.
- When the second is met, register 2 increments by one, and register 5 is reset to 1.
- The outer loop only ends when register 2 becomes greater than register 1 (which is fixed at 10551311
after the very early stages). When this happens, we soon get to instruction #16, which takes us out of bounds
and ends the program.

If you followed the above, you will see that the result held in register 0 at the end must be the sum of all
factors of 10551311. Which can be easily computed. (The number has exactly 2 distinct prime factors.)
-}
