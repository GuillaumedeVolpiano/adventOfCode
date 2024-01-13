module Day17
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, bounds, inRange, indices, (!))
import           Helpers.Graph      (Pos, dirs)
import           Helpers.Parsers    (make2DArray)
import           Intcode            (Intcode, clearOutput, execIntcode,
                                     initialise, outputASCIIcode, outputIntcode,
                                     sendASCIIInput, setMemory)
import           Linear.V2          (V2 (..))

type Scaffold = UArray Pos Char

a = "R,4,L,10,L,10\n"

b = "L,8,R,12,R,10,R,4\n"

c = "L,8,L,8,R,10,R,4\n"

routine = "A,B,A,B,A,C,B,C,A,C\n"

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

addInstruction :: String -> Intcode -> Intcode
addInstruction instruction = sendASCIIInput instruction . execIntcode

part1 :: Bool -> String -> String
part1 test
  | test = show . align . make2DArray . lines
  | otherwise =
    show .
    align .
    make2DArray . filter (not . null) . lines . outputASCIIcode . initialise

part2 :: Bool -> String -> String
part2 _ =
  show .
  head .
  outputIntcode .
  flip (foldr addInstruction) ["n\n", c, b, a, routine] .
  setMemory 0 2 . initialise
