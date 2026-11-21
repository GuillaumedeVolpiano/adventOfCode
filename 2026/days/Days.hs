-- |
-- Module      : Days
-- Description : The days of 2026, shared by the program, the tests and the
--               benchmarks.
module Days
  ( days
  ) where

import           Data.IntMap (IntMap, fromList)
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import           Helpers.Day (Day (Day))

-- | One line per day: the solvers, each followed by the answer to the
-- example given in the puzzle text (Nothing until it is known). Examples
-- are read from test/2026/dayN.txt, and from test/2026/dayNb.txt for part 2
-- when that file exists.
days :: IntMap Day
days =
  fromList
    [ (1, Day Day1.solve1 Nothing Day1.solve2 Nothing)
    , (2, Day Day2.solve1 Nothing Day2.solve2 Nothing)
    , (3, Day Day3.solve1 Nothing Day3.solve2 Nothing)
    , (4, Day Day4.solve1 Nothing Day4.solve2 Nothing)
    , (5, Day Day5.solve1 Nothing Day5.solve2 Nothing)
    , (6, Day Day6.solve1 Nothing Day6.solve2 Nothing)
    , (7, Day Day7.solve1 Nothing Day7.solve2 Nothing)
    , (8, Day Day8.solve1 Nothing Day8.solve2 Nothing)
    , (9, Day Day9.solve1 Nothing Day9.solve2 Nothing)
    , (10, Day Day10.solve1 Nothing Day10.solve2 Nothing)
    , (11, Day Day11.solve1 Nothing Day11.solve2 Nothing)
    , (12, Day Day12.solve1 Nothing Day12.solve2 Nothing)
    ]
