import qualified Data.Map.Strict as Map

type Recipe = Int
type ElfPositions = (Int, Int)
type PuzzleState = (Map.Map Int Recipe, ElfPositions)

newRecipes :: PuzzleState -> [Recipe]
-- compute the new recipes to be added to the list
newRecipes (rcps, (elf1Pos, elf2Pos)) = (map (read . pure)) $ show ((rcps Map.! elf1Pos) + (rcps Map.! elf2Pos))

elfMoves :: PuzzleState -> ElfPositions
-- compute the new positions of the elves after making the new recipes
elfMoves (rcps, (elf1Pos, elf2Pos)) = (newPos1, newPos2)
    where newPos1 = (elf1Pos + 1 + (rcps Map.! elf1Pos)) `mod` (Map.size rcps)
          newPos2 = (elf2Pos + 1 + (rcps Map.! elf2Pos)) `mod` (Map.size rcps)

updateState :: PuzzleState -> PuzzleState
-- combines the previous 2 functions into 1, to update the state in one go,
-- appending the new recipes to the old list
updateState st@(rcps, pos) = (recipes, elfMoves (recipes, pos))
    where maxKey = maximum $ Map.keys rcps
          recipes = fst $ foldr foldFunc (rcps, length $ newRecipes st) $ newRecipes st
          foldFunc rcp (map, idx) = (Map.insert (maxKey + idx) rcp map, idx - 1)

-- ran for around half an hour. (This was when I was working with lists, rather than maps. I thought
-- maps would work better due to quicker lookup, but the times I'm observing for "small" cases seem no
-- quicker)
first :: Int -> String
-- given a number of recipes, builds up the full list until there are that enough recipes,
-- then reads the next 10 recipes after the given one, as a string of digits
first numRecipes = concatMap show nextTen
    where initialState = (Map.fromList [(0, 3), (1, 7)], (0, 1))
          finalState state = if (Map.size $ fst state) >= (numRecipes + 10)
                                then state
                                else finalState $ updateState state
          nextTen = map snd $ take 10 $ drop numRecipes $ Map.toList $ fst $ finalState initialState

second :: String -> Int
-- takes a puzzle input as string (representing successive recipes),
-- and finds the number of recipes to the left of those. (Unsolved because has failed to give an answer in
-- over 2 hours!)
second recipeString = answer initialState
    where initialState = (Map.fromList [(0, 3), (1, 7)], (0, 1))
          answer state = if (take (length recipeString) (reverse $ map snd $ Map.toList (fst state))
                            == reverse (map (read . pure) recipeString))
                            then (Map.size $ fst state) - (length recipeString)
                            else answer $ updateState state
