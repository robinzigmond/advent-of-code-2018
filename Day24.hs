import Data.List (findIndex, isPrefixOf, sortOn, group)
import Data.Text (splitOn, pack, unpack)
import Data.Char (toUpper)
import Data.Function (on)

data DamageType = Radiation | Bludgeoning | Fire | Slashing | Cold deriving (Eq, Show, Read)
data Side = Immune | Infection deriving (Eq, Show, Read)
data Group = Group {idNum :: Int, side :: Side, numUnits :: Int, hitPoints :: Int, attackType :: DamageType,
                    attackDamage :: Int, initiative :: Int, immune :: [DamageType], weak :: [DamageType],
                    targeted :: Bool, target :: Maybe Int}
                deriving (Show)

instance Eq Group where
    (==) = (==) `on` idNum

parseLine :: Side -> Int -> String -> Group
-- extract the group information from a line of the puzzle input.
-- takes the side as a parameter because it's determined by the position of the line in the input, not by
-- the content of the line
parseLine grSide idNum str = Group idNum grSide grNumUnits grHitPoints grAttackType grAttackDamage grInitiative
                            grImmune grWeak False Nothing
    where split = words str
          numWords = length split
          capitaliseFirst word = toUpper (head word) : (tail word)
          leftBracketLocation = findIndex (=='(') str
          rightBracketLocation = findIndex (==')') str
          bracketedBit = if leftBracketLocation == Nothing
                            then Nothing
                            else let Just leftIdx = leftBracketLocation
                                     Just rightIdx = rightBracketLocation
                                     lengthOfIt = rightIdx - leftIdx - 1
                                 in Just $ take (lengthOfIt) $ drop (leftIdx + 1) str
          immuneAndWeak = if bracketedBit == Nothing
                            then []
                            else map unpack $ splitOn (pack "; ") (let Just text = bracketedBit in pack text)
          isImmune str = "immune to " `isPrefixOf` str
          isWeak str = "weak to " `isPrefixOf` str
          getTypes str = let offset = if isImmune str then 10 else 8
                         in map (read . capitaliseFirst . unpack) $ splitOn (pack ", ") (pack $ drop offset str)
          grNumUnits = read $ head split
          grHitPoints = read $ split!!4
          grAttackType = read $ capitaliseFirst $ split!!(numWords - 5)
          grAttackDamage = read $ split!!(numWords - 6)
          grInitiative = read $ split!!(numWords - 1)
          grImmune = let raw = filter (isImmune) immuneAndWeak
                     in if null raw then [] else getTypes $ head raw
          grWeak = let raw = filter (isWeak) immuneAndWeak
                   in if null raw then [] else getTypes $ head raw

parseWhileIncrementingID :: Int -> Side -> [String] -> [Group]
-- parses a set of strings representing groups of a particular side, and increments the id each time
parseWhileIncrementingID start side strs = map fancyParse [start..(start + length strs - 1)]
    where fancyParse n = parseLine side n $ strs!!(n - start)

parseFile :: String -> [Group]
-- get all the groups from the input file
parseFile input = (parseWhileIncrementingID 0 Immune immuneLines)
                    ++ (parseWhileIncrementingID 100 Infection infectionLines)
    where allLines = lines input
          theTwoParts = break (== pack "") $ map pack allLines
          immuneLines = tail $ map unpack $ fst theTwoParts
          infectionLines = tail $ tail $ map unpack $ snd theTwoParts

getData :: IO [Group]
getData = readFile "input24.txt" >>= (return . parseFile)

attackPower :: Group -> Int
-- simple helper function to determine a group's base attacking power
attackPower group = product $ [numUnits, attackDamage] <*> [group]

damageDone :: Group -> Group -> Int
-- determines how much damage one group would do to another
damageDone attack defence
    | side attack == side defence = 0
    | attackType attack `elem` immune defence = 0
    | attackType attack `elem` weak defence = 2 * attackPower attack
    | otherwise = attackPower attack

unitsLost :: Group -> Group -> Int
-- compute the number of units lost by a group when an attack comes in
unitsLost attacker target = (damageDone attacker target) `quot` (hitPoints target)

chooseTarget :: [Group] -> Group -> Maybe Int
-- find the group (if any) which the given group will choose to attack
chooseTarget groups attacker
    | null availableTargets = Nothing
    | numUnits attacker <= 0 = Nothing
    | otherwise = let ordered1 = reverse $ sortOn (damageDone attacker) availableTargets
                      potentials1 = head $ group ordered1
                  in if damageDone attacker (head potentials1) == 0
                        then Nothing
                        else if length potentials1 == 1
                                then Just $ idNum $ head potentials1
                                else let ordered2 = reverse $ sortOn attackPower potentials1
                                         potentials2 = head $ group ordered2
                                     in if length potentials2 == 1
                                            then Just $ idNum $ head potentials2
                                            else Just $ idNum $ last $ sortOn initiative potentials2

    where availableTargets = filter attackable groups
          attackable tgt = (side tgt /= side attacker) && (not $ targeted tgt) && (numUnits tgt > 0)

doTargeting :: [Group] -> [Group]
-- assigns targets to all groups, in the appropriate order
doTargeting groups = doRound (length groups - 1) powerOrder
    where powerOrder = let attempt = sortOn attackPower groups
                       in if any ((>1) . length) $ group attempt
                            then concatMap (reverse . (sortOn initiative)) $ group attempt
                            else attempt
          doRound n groups
            | n < 0 = groups
            | otherwise = case chooseTarget groups (groups!!n) of
                            Nothing -> doRound (n-1) groups
                            Just tgt -> doRound (n-1) $ map (\grp -> if grp == (groups!!n)
                                                        then grp {target=Just tgt}
                                                        else if idNum grp == tgt
                                                                then grp {targeted=True}
                                                                else grp ) groups

winner :: [Group] -> Maybe Side
-- determines if the battle is still going on, and if it's over, who the winner is
winner groups
    | null (survivors Immune) = Just Infection
    | null (survivors Infection) = Just Immune
    | otherwise = Nothing
    where survivors team = filter (\group -> (side group == team) && (numUnits group > 0)) groups

doAttack :: [Group] -> Group -> [Group]
-- computes the result of a single attack by one group
doAttack groups attacker
    | theTarget == Nothing = groups
    | otherwise = let Just tgt = theTarget
                      mapping grp = if idNum grp == tgt
                                        then let unitsLeft = max 0 $ numUnits grp - (unitsLost attacker grp)
                                             in grp {numUnits=unitsLeft}
                                        else grp
                  in map mapping groups
    where theTarget = target attacker

battleRound :: [Group] -> [Group]
-- computes the result of an entire round of battle
battleRound groups = doRound 0 initiativeOrder
    where initiativeOrder = reverse $ sortOn initiative $ doTargeting groups
          reset = map (\grp -> grp {targeted=False, target=Nothing})
          doRound n groups
            | winner groups /= Nothing = groups
            | n >= length groups = reset groups
            | otherwise = let attacker = groups!!n in doRound (n+1) $ doAttack groups attacker

battleResult :: [Group] -> [Group]
-- runs numerous rounds of the battle, until there is a winner
battleResult groups
    | winner groups == Nothing = battleResult $ battleRound groups
    | otherwise = groups

solveFirst :: [Group] -> Int
solveFirst = sum . (map numUnits) . battleResult

first :: IO ()
first = getData >>= (putStrLn . show . solveFirst)

boost :: [Group] -> Int -> [Group]
-- give each Immune group a boost as described in the puzzle
boost groups amount = map doBoost groups
    where doBoost group = if side group == Immune
                            then let power = attackDamage group in group {attackDamage=power+amount}
                            else group

resultGivenBoost :: [Group] -> Int -> Maybe Side
resultGivenBoost groups = winner . battleResult . (boost groups)

smallestBoost :: [Group] -> Int
-- compute smallest boost required to allow Immune to win the battle
smallestBoost groups = head $ dropWhile ((/= Just Immune) . (resultGivenBoost groups)) [1..]

solveSecond :: [Group] -> Int
solveSecond groups = sum $ (map numUnits) $ battleResult $ boost groups $ smallestBoost groups

second :: IO ()
second = getData >>= (putStrLn . show . solveSecond)
-- ran for too long - well, around 10 minutes, before I realised I had a quicker way to do this, and that
-- rather than code it I might as well do it "by hand" :)

-- alternative allowing values to be tested interactively, for a "divide and conquer" approach. Using this I
-- quickly determined that the smallest boost required was somewhere in the 110-120 range.
-- But it gets very slow for values around here, I guess those battles are long and close :-)
-- (Turned out this wasn't quite right, see below...)
testBoost :: Int -> IO ()
testBoost n = getData >>= (putStrLn . show . (flip resultGivenBoost n))

-- well, it's just going to take a while. Going to at least get it to show me the latest progress :)
testAndPrint :: IO ()
testAndPrint = getData >>= (\groups -> doIt groups 0)
    where doIt groups n = do
            putStrLn $ "Giving boost of " ++ (show n)
            case resultGivenBoost groups n of
                Just Immune -> do
                            putStrLn "Immune wins!"
                            putStrLn $ "Units left - " ++ show (sum $ map numUnits $ battleResult $ boost groups n)
                Just Infection -> do
                            putStrLn "Infection wins!"
                            doIt groups (n+1)
                Nothing -> do -- can't actually happen, due to how we've defined the function that gives the result!
                            putStrLn "it's a tie!"
                            doIt groups (n+1)
-- this shows a boost of 112 taking FAR too long (over 2 hours so far!). I suppose it's theoretically possible
-- for a game to end in stalemate, if the only surviving groups are each resistant to all the opposing groups'
-- attacks. (This of course wouldn't count as a victory for anyone, but would block output from being produced.)

-- But I can't assume a LOOOONNNG process time necessarily means this. It could also mean that the battle has
-- entered a very slow endgame, where both groups have a large amount of life left and only a very small amount is
-- being taken off each round. In order to see what's going on, I've writte the below function to display the
-- successive stages of combat (for a given boost):

showProgress :: Int -> IO ()
showProgress n = getData >>= (\groups -> doIt 0 (boost groups n))
    where doIt n groups = do
            let living = filter (\grp -> numUnits grp > 0) groups
            let livingImmune = filter (\grp -> side grp == Immune) living
            let livingInfection = filter (\grp -> side grp == Infection) living
            let details grp = "id: " ++ (show $ idNum grp) ++  ", units: " ++ (show $ numUnits grp)
            putStrLn $ "after " ++ (show n) ++ " battles"
            putStrLn "detail of current groups: " 
            putStrLn $ show (length living) ++  (" still alive -")
            putStrLn $ show (length livingImmune) ++ " immune and"
            putStrLn $ show (length livingInfection) ++ " infection"
            putStrLn $ show $ map details living
            if map numUnits (sortOn idNum (battleRound groups)) == map numUnits (sortOn idNum groups)
                then do 
                        putStrLn "stalemate! - result:"
                        putStrLn $ show $ winner groups
                else doIt (n+1) $ battleRound groups
-- running this with 112 makes it clear that there is indeed a stalemate!
-- We end with 1 immune unit and 2 infection ones. The Immune unit is "immune" (in the other sense) to the fire
-- attack used by both the surviving Infection units. And although they aren't immune to its Radiation attack, the
-- damage, with this boost anyway, is not enough to cause any units to be lost. So stalemate is indeed the result.

-- exactly the same thing (same groups, although different final unit numbers) happens for a boost of 113. And 114.
-- But for 115, we go back to an outright infection win! (It has 2 groups left, the same 2 as featured in
-- the stalemantes for 112-114. I'm not sure why increasing the boost makes the Immune unit die, but there are
-- a few possible explanations, and I don't care to look in more detail now as to the precise reasons.)

-- 116 is also stalemate, this time with 3 surviving groups on each side
-- so is 117, but with *5* Immune groups and only 1 Infection one. Victory seems close!
-- not on 118 though. Similar outcome. But victory comes at 119! I get a value of 5518 (confirmed by
-- running the "second" function above and changing the lower bound to 119) - but it's not accepted :(

-- Well I've double and triple-checked, and can't find anything wrong with my conclusions, which are:
-- - with boosts of 111 or less, Infection wins
-- - boosts of 112-114 reach a "stalemate", where no surviving group can deal enough damage to the opposing ones
-- to remove any units
-- - if the boost is 115, strangely, Infection goes back to winning. (Not impossible, and I guess is due to
-- one Infection unit staying alive long enough to be targeted instead of one which is actually a bigger threat)
-- - boosts of 116-118 again result in stalemates
-- - with a boost of 119, Immune finally wins. And with 5518 units left over.

-- since this answer is apparently wrong, the only remaining conclusion is that some part of my program is wrong
-- that would be very unusual, given that it achieves the correct answer for the first question, and the correct
-- battle sequence and intermediate results for the example given for Puzzle 2
