-- very much incomplete, in fact barely started!

import Data.Function (on)

type Position = (Int, Int)
data Door = North | East | South | West deriving (Eq, Show)
data Room = Room {position :: Position, doors :: [Door]} deriving Show
type Maze = [Room]

-- it will be convenient to recognise that rooms are the same whenever they have the same location
instance Eq Room where
    (==) = (==) `on` position

getData :: IO String
getData = readFile "input20.txt"

-- the main part of the puzzle is of course parsing the "regex" into the room layout (Maze value, using the types
-- defined above). In order to do this, we need to define some more types which are specific to the parsing
-- process and will be used internally:

-- this type represents a set of directions starting from a specific point
data Path = Path {dirs :: [Door], start :: Position} deriving (Eq, Show)

-- this one represents the information we need to keep track of while parsing:
data ParseState = ParseState {textPos :: Int, completePaths :: [Path], openPaths :: [Path], currentPath :: Path}
    deriving (Eq, Show)

getDir :: Char -> Door
getDir 'N' = North
getDir 'E' = East
getDir 'S' = South
getDir 'W' = West
getDir x = error $ "invalid character for direction: " ++ [x] 

goThroughDoor :: Position -> Door -> Position
goThroughDoor (x, y) door = case door of
                                North -> (x, y-1)
                                East -> (x+1, y)
                                South -> (x, y+1)
                                West -> (x-1, y)

followPath :: Path -> Position
followPath (Path [] start) = start
followPath (Path (d:ds) start) = followPath (Path ds (goThroughDoor start d))

parseChar :: Char -> ParseState -> ParseState
-- updates the parse state based on the character being scanned
-- algorithem as follows:
-- 1) If a direction (NESW), add it to the current path
-- 2) If an open bracket, start a new subpath to the (previously) current one, and make it current
-- 3) If a pipe, close the subpath, and start a new one with the same "parent"
-- 4) if a closed bracket, close the subpath and move back up to the parent
-- (we don't specify anything for ^ and $, which delimit the string, we'll deal with that when
-- parsing an entire string in the next function)
-- [This can leave an empty path, this is find, and fits in with the Path datatype above]
parseChar chr (ParseState {textPos=pos, completePaths=done, openPaths=open, currentPath=current})
    = ParseState (pos+1) nowDone nowOpen nowCurrent
    where nowDone = case chr of
                        '|' -> current:done
                        ')' -> current:done
                        _ -> done
          addChar char (Path{dirs=xs, start=pos}) = Path ((getDir char):xs) pos
          newPath posn = Path [] posn
          nowOpen = case chr of
                        '(' -> current:open
                        '|' -> current:open
                        ')' -> if length open > 1 then tail open else []
                        _ -> open
          nowCurrent = case chr of
                        'N' -> addChar 'N' current
                        'E' -> addChar 'E' current
                        'S' -> addChar 'S' current
                        'W' -> addChar 'W' current
                        '(' -> newPath $ followPath current
                        '|' -> newPath $ start current

-- need to rethink, this only gives paths, not rooms (with doors) that we need for the maze!
