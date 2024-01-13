module Day17
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, bounds, inRange, indices, (!))
import           Helpers.Graph      (Pos, dirs)
import           Helpers.Parsers    (make2DArray)
import           Intcode            (Intcode, clearOutput, execASCIIcode,
                                     initialise, runASCIIcode, setMemory)
import           Linear.V2          (V2 (..))

type Scaffold = UArray Pos Char

align :: Scaffold -> Int
align scaffold =
  sum .
  map (\(V2 x y) -> x * y) .
  filter
    (\p ->
       scaffold ! p == '#' &&
       all (\d -> inRange b (p + d) && scaffold ! (p + d) == '#') dirs) .
  indices $
  scaffold
  where
    b = bounds scaffold

part1 :: Bool -> String -> String
part1 test
  | test = show . align . make2DArray . lines
  | otherwise =
    show .
    align .
    make2DArray . filter (not . null) . lines . execASCIIcode . initialise

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
