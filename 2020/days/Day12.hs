module Day12
  ( part1
  , part2
  ) where

import           Helpers.Graph (Pos, east, north, south, west)
import           Linear.V2     (V2 (..))
import           Linear.Vector ((*^))

type Instruction = (Char, Int)

type Ship = (Pos, Bearing)

type Bearing = Pos

left :: Int -> Pos -> Pos
left 0 b        = b
left v (V2 x y) = left (v - 90) (V2 y (-x))

right :: Int -> Pos -> Pos
right 0 b        = b
right v (V2 x y) = right (v - 90) (V2 (-y) x)

move1 :: Ship -> Instruction -> Ship
move1 (pos, bearing) (order, val)
  | order == 'N' = (pos + val *^ north, bearing)
  | order == 'S' = (pos + val *^ south, bearing)
  | order == 'E' = (pos + val *^ east, bearing)
  | order == 'W' = (pos + val *^ west, bearing)
  | order == 'L' = (pos, left val bearing)
  | order == 'R' = (pos, right val bearing)
  | order == 'F' = (pos + val *^ bearing, bearing)

manhattanDistance :: Ship -> Int
manhattanDistance (V2 x y, _) = abs x + abs y

move2 :: Ship -> Instruction -> Ship
move2 (pos, bearing) (order, val)
  | order == 'N' = (pos, bearing + val *^ north)
  | order == 'S' = (pos, bearing + val *^ south)
  | order == 'E' = (pos, bearing + val *^ east)
  | order == 'W' = (pos, bearing + val *^ west)
  | order == 'L' = (pos, left val bearing)
  | order == 'R' = (pos, right val bearing)
  | order == 'F' = (pos + val *^ bearing, bearing)

part1 :: Bool -> String -> String
part1 _ =
  show .
  manhattanDistance .
  foldl move1 (V2 0 0, east) . map (\(a:b) -> (a, read b)) . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  manhattanDistance .
  foldl move2 (V2 0 0, V2 10 (-1)) . map (\(a:b) -> (a, read b)) . lines
