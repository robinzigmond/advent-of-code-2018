{- I cheated a little bit here - after my naive solution to part (naively sing lists) took over half an hour,
and I couldn't see any "trick" which would reduce the solution time significantly (to allow me to have any
hope of part 2 in less than a few hours), I read a few discussion threads on this puzzle where it was indicated
that the key was just to use the right data structure. So after some research I decided to use Sequences instead,
since these can do things like insert into the list in logarithmic time (rather than linear). This improved
things greatly - although not as dramatically as I'd hoped - in that the time for part 1 reduced to "just"
5 minutes. Unfortunately, since the total time is clearly slightly worse than O(n), that would still make
Part 2 infeasible in reasonable time.

Further, I'm getting stack overflows when I try to run it. After more research, it seems that in Haskell this
is caused by lazy evaluation and I should use the $! operator to force evaluation of intermediate results.
Sadly even doing this I am still getting stack overflows a few minutes into the computation. 

I guess I can switch to another data structure (Vectors will be my next attempt), in the interest of speed,
but I don't see how this will solve the overflow problem -}

import qualified Data.Map as Map
import Data.Sequence

numPlayers = 423

lastMarble = 71944

type Marble = Int
type Scores = Map.Map Int Int

data GameState = GameState {marbles :: Seq Marble, scores :: Scores, currentMarble :: Marble, nextMarble :: Marble}
                    deriving (Show)

initialState = GameState (singleton 0) Map.empty 0 1

move :: Int -> Marble -> Seq Marble -> Marble
-- utility function to move, cyclically, around the circle of marbles, from a given initial marble
move n marble marbles = let Just idx = elemIndexL marble marbles
                        in marbles `index` ((idx + n) `mod` (Data.Sequence.length marbles))

doTurn :: GameState -> GameState
doTurn GameState {marbles=currMarbles, scores=currScores, currentMarble=currPosition, nextMarble=next}
    = GameState {marbles=newMarbles, scores=newScores, currentMarble=nextPosition, nextMarble=next+1}
    where newMarbles = if next `mod` 23 == 0
                        then let toRemove = move (-7) currPosition currMarbles
                                 Just deleteIndex = elemIndexL toRemove currMarbles
                             in deleteAt deleteIndex currMarbles
                        else let Just idx = elemIndexL currPosition currMarbles
                                 insertIndex = (idx + 2) `mod` (Data.Sequence.length currMarbles)
                             in insertAt insertIndex next currMarbles
          newScores = if next `mod` 23 == 0
                        then let player = next `mod` numPlayers
                                 scored = next + move (-7) currPosition currMarbles
                                 oldScore = case Map.lookup player currScores of
                                                Nothing -> 0
                                                Just score -> score
                                 newScore = scored + oldScore
                             in Map.insert player newScore currScores
                        else currScores
          nextPosition = if next `mod` 23 == 0
                            then move (-6) currPosition currMarbles
                            else next

maxScore :: Scores -> Int
maxScore scores = maximum $ map getScore [1..numPlayers]
            where getScore player = case Map.lookup player scores of
                                        Nothing -> 0
                                        Just score -> score

solveFirst :: Int -> Int
solveFirst last = maxScore $ scores $ apply last doTurn initialState
                     where apply n f x = if n == 0
                                            then x
                                            else if n == 1
                                                    then f x
                                                    else f $! apply (n-2) f (f x)

first :: IO ()
first = putStrLn $ show $ solveFirst lastMarble

solveSecond :: Int
solveSecond = solveFirst (100 * lastMarble)

second :: IO ()
second = putStrLn $ show $ solveSecond
