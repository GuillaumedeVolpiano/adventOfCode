module Day22 (part1, part2) where
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Array.Unboxed (UArray, array, bounds, inRange, (!))
import           Data.List          (uncons)
import           Data.Maybe         (Maybe (Just, Nothing), fromJust, isNothing)
import           Linear.V2          (V2 (..))

import           Debug.Trace

data State =
  State
    { board        :: Board
    , pos          :: Pos
    , dir          :: Dir
    , instructions :: [Instruction]
    , part         :: Int
    , isTest       :: Bool
    }
  deriving (Show)

type Board = UArray Pos Char

type Pos = V2 Int

type Dir = Pos

data Instruction
  = Rotate Char
  | Move Int
  deriving (Show, Eq)

parseInput :: String -> Int -> Bool -> State
parseInput i = State b (findPos (V2 1 1)) (V2 1 0) (parseIns rawIns)
  where
    li = lines i
    rawIns = last li
    rawBoard = init . init $ li
    height = length rawBoard
    width = maximum . map length $ rawBoard
    b =
      array
        (V2 1 1, V2 width height)
        [ if x - 1 < length (rawBoard !! (y - 1))
          then (V2 x y, rawBoard !! (y - 1) !! (x - 1))
          else (V2 x y, ' ')
        | x <- [1 .. width]
        , y <- [1 .. height]
        ]
    findPos v
      | not (inRange (bounds b) v) =
        error ("v is not in range " ++ show v ++ "\n" ++ show b)
      | b ! v == '.' = v
      | otherwise = findPos $ v + V2 1 0
    parseIns line
      | null line = []
      | line =~ "^[LR]" :: Bool = Rotate (head line) : parseIns (tail line)
      | otherwise = Move (read this) : parseIns after
      where
        (_, this, after) = line =~ "^[0-9]+" :: (String, String, String)

right :: Dir -> Dir
right (V2 x y) = V2 (-y) x

left :: Dir -> Dir
left (V2 x y) = V2 y (-x)

rotate :: State -> State
rotate state = state {instructions = tail (instructions state), dir = newDir}
  where
    newDir
      | head (instructions state) == Rotate 'L' = left . dir $ state
      | head (instructions state) == Rotate 'R' = right . dir $ state
      | otherwise =
        error
          ("rotate should only be used on a Rotate instruction. " ++
           show (head . instructions $ state) ++ "\n" ++ show state)

isRotate :: Instruction -> Bool
isRotate (Rotate _) = True
isRotate _          = False

overEdge :: State -> Maybe State
overEdge state
  | not (inRange (bounds curBoard) newPos) =
    error
      ("newPos is not in range! " ++
       show (pos state) ++
       " " ++ show newPos ++ " " ++ show (bounds . board $ state))
  | curBoard ! newPos == '#' = Nothing
  | curBoard ! newPos == '.' = Just state {pos = newPos, dir = newDir}
  | part state == 1 && curBoard ! newPos == ' ' = overSpace state {pos = newPos}
  | otherwise =
    error
      ("It would seem we messed up the calcualtion of the new position. " ++
       show (pos state) ++
       " " ++ show curDir ++ " " ++ show newPos ++ " " ++ show newDir)
  where
    (V2 minx miny, V2 maxx maxy) = bounds . board $ state
    (V2 x y) = pos state
    curBoard = board state
    curDir = dir state
    newDir
      | part state == 1 = curDir
      | part state == 2 && isTest state = newDirTest
      | part state == 2 = newDirActual
    newPos
      | part state == 1 = simpleNewPos
      | part state == 2 && isTest state = newPosTest
      | part state == 2 = newPosActual
    simpleNewPos
      | x < minx && dir state == V2 (-1) 0 = V2 maxx y
      | x > maxx && dir state == V2 1 0 = V2 minx y
      | y < miny && dir state == V2 0 (-1) = V2 x maxy
      | y > maxy && dir state == V2 0 1 = V2 x miny
      | otherwise =
        error
          ("This state does not seem to be going over the edge.\n" ++ show state)
    newDirTest
      | y < 1 && curDir == V2 0 (-1) = V2 0 1
      | y == 4 && x < 5 && curDir == V2 0 (-1) = V2 0 1
      | y == 4 && x < 9 && curDir == V2 0 (-1) = V2 1 0
      | y == 8 && x > 12 && curDir == V2 0 (-1) = V2 (-1) 0
      | y == 9 && x < 5 && curDir == V2 0 1 = V2 0 (-1)
      | y == 9 && x < 9 && curDir == V2 0 1 = V2 1 0
      | y > 12 && x < 13 && curDir == V2 0 1 = V2 0 (-1)
      | y > 12 && curDir == V2 0 1 = V2 1 0
      | x == 13 && y < 5 && curDir == V2 1 0 = V2 (-1) 0
      | x == 13 && y < 9 && curDir == V2 1 0 = V2 0 1
      | x > 16 && curDir == V2 1 0 = V2 (-1) 0
      | x == 8 && y < 5 && curDir == V2 (-1) 0 = V2 1 0
      | x < 1 && curDir == V2 (-1) 0 = V2 0 (-1)
      | x == 8 && y > 8 && curDir == V2 (-1) 0 = V2 0 (-1)
      | otherwise =
        error
          ("It seems that this exit is not coded. " ++
           show (pos state) ++ " " ++ show curDir)
    newDirActual
     -- 1l -> 4l
      | x == 50 && y < 51 && curDir == V2 (-1) 0 = V2 1 0
    -- 1u -> 6l
      | y == 0 && x < 101 && curDir == V2 0 (-1) = V2 1 0
    -- 2d -> 3r
      | y == 51 && curDir == V2 0 1 = V2 (-1) 0
    -- 2r -> 5r
      | x == 151 && curDir == V2 1 0 = V2 (-1) 0
    -- 2u -> 6d
      | y == 0 && x > 100 && curDir == V2 0 (-1) = V2 0 (-1)
    -- 3l -> 4u
      | x == 50 && curDir == V2 (-1) 0 = V2 0 1
    -- 3r -> 2d
      | x == 101 && y < 101 && curDir == V2 1 0 = V2 0 (-1)
    -- 4l -> 1l
      | x == 0 && y < 151 && curDir == V2 (-1) 0 = V2 1 0
    -- 4u -> 3l
      | y == 100 && x < 51 && curDir == V2 0 (-1) = V2 1 0
    -- 5d -> 6r
      | y == 151 && curDir == V2 0 1 = V2 (-1) 0
    -- 5r -> 2r
      | x == 101 && curDir == V2 1 0 = V2 (-1) 0
    -- 6l -> 1u
      | x == 0 && curDir == V2 (-1) 0 = V2 0 1
    -- 6r -> 5d
      | x == 51 && curDir == V2 1 0 = V2 0 (-1)
    -- 6d -> 2u
      | y == 201 && curDir == V2 0 1 = V2 0 1
      | otherwise =
        error
          ("This position is not coded. " ++
           show (pos state) ++ " " ++ show curDir)
    newPosTest
      | y < 1 && curDir == V2 0 (-1) = V2 (13 - x) 5
      | y == 4 && x < 5 && curDir == V2 0 (-1) = V2 (13 - x) 1
      | y == 4 && x < 9 && curDir == V2 0 (-1) = V2 9 (x - 4)
      | y == 8 && x > 12 && curDir == V2 0 (-1) = V2 12 (21 - x)
      | y == 9 && x < 5 && curDir == V2 0 1 = V2 (13 - x) 12
      | y == 9 && x < 9 && curDir == V2 0 1 = V2 12 (21 - x)
      | y > 12 && x < 13 && curDir == V2 0 1 = V2 (13 - x) 8
      | y > 12 && curDir == V2 0 1 = V2 1 (21 - x)
      | x == 13 && y < 5 && curDir == V2 1 0 = V2 16 (17 - y)
      | x == 13 && y < 9 && curDir == V2 1 0 = V2 (21 - y) 9
      | x > 16 && curDir == V2 1 0 = V2 12 (17 - y)
      | x == 8 && y < 5 && curDir == V2 (-1) 0 = V2 (y + 4) 5
      | x < 1 && curDir == V2 (-1) 0 = V2 (17 - y) 12
      | x == 8 && y > 8 && curDir == V2 (-1) 0 = V2 (17 - y) 8
      | otherwise =
        error
          ("It seems that this exit is not coded. " ++
           show (pos state) ++ " " ++ show curDir)
    newPosActual
      -- 1u -> 6l
      | y == 0 && x < 101 && curDir == V2 0 (-1) = V2 1 (x + 100)
      -- 2u -> 6d
      | y == 0 && curDir == V2 0 (-1) = V2 (x - 100) 200
      -- 4u -> 3l
      | y == 100 && curDir == V2 0 (-1) = V2 51 (x + 50)
      -- 2d -> 3r
      | y == 51 && curDir == V2 0 1 = V2 100 (x - 50)
      -- 5d -> 6r
      | y == 151 && curDir == V2 0 1 = V2 50 (x + 100)
      -- 6d -> 2u
      | y == 201 && curDir == V2 0 1 = V2 (x + 100) 1
      -- 2r -> 5r
      | x == 151 && curDir == V2 1 0 = V2 100 (151 - y)
      -- 3r -> 2d
      | x == 101 && y < 101 && curDir == V2 1 0 = V2 (y + 50) 50
      -- 5r -> 2r
      | x == 101 && curDir == V2 1 0 = V2 150 (151 - y)
      -- 6r -> 5d
      | x == 51 && curDir == V2 1 0 = V2 (y - 100) 150
      -- 1l -> 4l
      | x == 50 && y < 51 && curDir == V2 (-1) 0 = V2 1 (151 - y)
      -- 3l -> 4u
      | x == 50 && curDir == V2 (-1) 0 = V2 (y - 50) 101
      -- 4l -> 1l
      | x == 0 && y < 151 && curDir == V2 (-1) 0 = V2 51 (151 - y)
      -- 6l -> 1u
      | x == 0 && curDir == V2 (-1) 0 = V2 (y - 100) 1
      | otherwise =
        error
          ("This position is not coded. " ++
           show (pos state) ++ " " ++ show curDir)

overSpace :: State -> Maybe State
overSpace state
  | (part state == 2) || not (inRange (bounds . board $ state) $ pos state) =
    overEdge state
  | board state ! pos state == ' ' =
    overSpace state {pos = pos state + dir state}
  | board state ! pos state == '#' = Nothing
  | board state ! pos state == '.' = Just state

move :: State -> State
move state
  | null $ instructions state = state
  | isRotate curInst = move . rotate $ state
  | curInst == Move 0 = move state {instructions = newInst}
  | not (inRange (bounds curBoard) newPos) &&
      isNothing (overEdge state {pos = newPos}) =
    move state {instructions = newInst}
  | not (inRange (bounds curBoard) newPos) =
    move $
    fromJust
      (overEdge state {pos = newPos, instructions = dim curInst : newInst})
  | ((curBoard ! newPos == ' ') && isNothing (overSpace state {pos = newPos})) ||
      (curBoard ! newPos == '#') = move state {instructions = newInst}
  | curBoard ! newPos == '.' =
    move state {instructions = dim curInst : newInst, pos = newPos}
  | curBoard ! newPos == ' ' =
    move $
    fromJust
      (overSpace state {pos = newPos, instructions = dim curInst : newInst})
  where
    (curInst, newInst) = fromJust . uncons . instructions $ state
    curBoard = board state
    newPos = pos state + dir state
    dim (Move x) = Move (x - 1)

password :: State -> Int
password state = r * 1000 + c * 4 + f
  where
    (V2 c r) = pos state
    f
      | dir state == V2 1 0 = 0
      | dir state == V2 0 1 = 1
      | dir state == V2 (-1) 0 = 2
      | dir state == V2 0 (-1) = 3

part1 :: Bool -> String -> String
part1 test input = show . password . move $ parseInput input 1 test

part2 :: Bool -> String -> String
part2 test input = show . password . move $ parseInput input 2 test
