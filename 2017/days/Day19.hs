module Day19
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, assocs, (!?))
import           Data.Char          (isAlpha)
import           Data.Maybe         (fromJust, isJust, isNothing)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

type Maze = UArray Pos Char

data Packet = Packet
  { pos     :: Pos
  , dir     :: Pos
  , seen    :: String
  , counter :: Int
  } deriving (Show)

left :: Pos -> Pos
left (V2 x y) = V2 y (-x)

right :: Pos -> Pos
right (V2 x y) = V2 (-y) x

runMaze1 :: Maze -> String
runMaze1 maze = reverse . seen . explore maze $ packet
  where
    [(start, _)] = filter isStart . assocs $ maze
    isStart (V2 _ y, c) = y == 0 && c == '|'
    packet = Packet start (V2 0 1) "" 0

runMaze2 :: Maze -> Int
runMaze2 maze = counter . explore maze $ packet
  where
    [(start, _)] = filter isStart . assocs $ maze
    isStart (V2 _ y, c) = y == 0 && c == '|'
    packet = Packet start (V2 0 1) "" 0

explore :: Maze -> Packet -> Packet
explore maze packet
  | isNothing safePath = error "I'm all lost in the supermarket"
  | path == ' ' = packet
  | path `elem` "|-" = explore maze . move $ packet
  | path == '+' = explore maze . move . fromJust . rotate $ packet
  | isAlpha path = explore maze . move . checkMove . collect $ packet
  where
    safePath = maze !? pos packet
    path = fromJust safePath
    move p = p {pos = pos p + dir p, counter = counter p + 1}
    rotate p
      | null turns || length turns == 2 = Nothing
      | otherwise = Just p {dir = head turns}
    turns = filter canTurn [left . dir $ packet, right . dir $ packet]
    canTurn d = isJust (nextStep d) && nextStep d /= Just ' '
    nextStep = (maze !?) . (+ pos packet)
    collect p = p {seen = path : seen packet}
    checkMove p
      | (dir p `elem` [V2 0 1, V2 0 (-1)]
           && fromJust (maze !? (dir p + pos p)) `elem` "+|")
          || (dir p `elem` [V2 1 0, V2 (-1) 0]
                && fromJust (maze !? (dir p + pos p)) `elem` "+-") = p
      | isJust . rotate $ p = fromJust . rotate $ p
      | otherwise = p

part1 :: Bool -> String -> String
part1 _ = runMaze1 . arrayFromString

part2 :: Bool -> String -> String
part2 _ = show . runMaze2 . arrayFromString
