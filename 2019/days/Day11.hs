module Day11
  ( part1
  , part2
  ) where

import           Data.HashMap.Lazy as M (HashMap, findWithDefault, insert, keys,
                                         singleton, size)
import           Data.List.Split   (chunksOf)
import           Helpers.Parsers   (Pos)
import           Intcode           (Intcode, halted, initialise, runIntcode,
                                    sendInput)
import           Linear.V2         (V2 (..))

data State =
  State Robot Hull Intcode

type Hull = HashMap Pos Int

type Robot = (Pos, Pos)

left :: Pos -> Pos
left (V2 x y) = V2 y (-x)

right :: Pos -> Pos
right (V2 x y) = V2 (-y) x

move :: State -> Hull
move (State (pos, dir) hull machine)
  | halted machine = hull
  | otherwise = move . State (newPos, newDir) newHull $ newMachine
  where
    input = findWithDefault 0 pos hull
    (output, newMachine) = runIntcode . sendInput input $ machine
    colour = output !! 1
    direction = head output
    newHull = insert pos colour hull
    newDir
      | direction == 0 = left dir
      | direction == 1 = right dir
    newPos = pos + newDir

makeState :: Int -> String -> State
makeState startPos code = State (newDir, newDir) hull machine
  where
    (firstOutput, machine) = runIntcode . sendInput startPos . initialise $ code
    colour = firstOutput !! 1
    direction = head firstOutput
    hull = singleton (V2 0 0) colour
    newDir
      | direction == 0 = left (V2 0 (-1))
      | direction == 1 = right (V2 0 (-1))

render :: Hull -> String
render hull =
  unlines . chunksOf width $
  [ charify . findWithDefault 0 (V2 x y) $ hull
  | y <- [my .. mY]
  , x <- [mx .. mX]
  ]
  where
    painted = keys hull
    xs = map (\(V2 x _) -> x) painted
    ys = map (\(V2 _ y) -> y) painted
    mx = minimum xs
    mX = maximum xs
    my = minimum ys
    mY = maximum ys
    width = mX - mx + 1
    charify 1 = '#'
    charify 0 = ' '

part1 :: Bool -> String -> String
part1 _ = show . size . move . makeState 0

part2 :: Bool -> String -> String
part2 _ = render . move . makeState 1
