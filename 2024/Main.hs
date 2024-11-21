{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Data.Map                        (Map, empty, fromList, (!))
import           Data.Time.Calendar              (toGregorian)
import           Data.Time.Clock                 (getCurrentTime, utctDay)
import           Day1
import           Day10
import           Day11
import           Day12
import           Day13
import           Day14
import           Day15
import           Day16
import           Day17
import           Day18
import           Day19
import           Day2
import           Day20
import           Day21
import           Day22
import           Day23
import           Day24
import           Day25
import           Day3
import           Day4
import           Day5
import           Day6
import           Day7
import           Day8
import           Day9
import           Helpers.General                 (customPreciseTimeIt,
                                                  retrieveInput, wallTimeIt)
import           System.Console.CmdArgs.Implicit (Data, Typeable, args, cmdArgs,
                                                  def, help, opt, (&=))

data Arguments = Arguments
  { day         :: Int
  , test        :: Bool
  , proxy       :: Bool
  , wallTime    :: Bool
  , interactive :: Bool
  } deriving (Show, Data, Typeable)

type Solver = (Bool -> String -> String)

type Day = (Solver, Solver)

solver =
  fromList
    [ (1, (Day1.part1, Day1.part2))
    , (2, (Day2.part1, Day2.part2))
    , (3, (Day3.part1, Day3.part2))
    , (4, (Day4.part1, Day4.part2))
    , (5, (Day5.part1, Day5.part2))
    , (6, (Day6.part1, Day6.part2))
    , (7, (Day7.part1, Day7.part2))
    , (8, (Day8.part1, Day8.part2))
    , (9, (Day9.part1, Day9.part2))
    , (10, (Day10.part1, Day10.part2))
    , (11, (Day11.part1, Day11.part2))
    , (12, (Day12.part1, Day12.part2))
    , (13, (Day13.part1, Day13.part2))
    , (14, (Day14.part1, Day14.part2))
    , (15, (Day15.part1, Day15.part2))
    , (16, (Day16.part1, Day16.part2))
    , (17, (Day17.part1, Day17.part2))
    , (18, (Day18.part1, Day18.part2))
    , (19, (Day19.part1, Day19.part2))
    , (20, (Day20.part1, Day20.part2))
    , (21, (Day21.part1, Day21.part2))
    , (22, (Day22.part1, Day22.part2))
    , (23, (Day23.part1, Day23.part2))
    , (24, (Day24.part1, Day24.part2))
    , (25, (Day25.part1, Day25.part2))
    ]

interSolver = empty

main :: IO ()
main = do
  time <- getCurrentTime
  let (_, _, curDay) = toGregorian . utctDay $ time
      arguments =
        Arguments
          { day =
              def
                &= help "Which day to process. Defaults to the current day"
                &= opt curDay
          , test = def &= help "Run the test suite? Defaults to True if used"
          , proxy = def &= help "Use a proxy? Defaults to True if used"
          , wallTime = def &= help "Report wall Time rather than CPU time"
          , interactive = def &= help "Run interactively"
          }
  args <- cmdArgs arguments
  let year = 2024
      theDay =
        case day args of
          0 -> curDay
          _ -> day args
  input <- retrieveInput year theDay (test args) True (proxy args)
  let (solve1, solve2) = solver ! theDay
      timer =
        if wallTime args
          then wallTimeIt
          else customPreciseTimeIt
      result
        | interactive args = interSolver ! theDay $ input
        | otherwise = do
          timer "Part 1." 4 . putStrLn $ solve1 (test args) input
          timer "Part 2." 4 . putStrLn $ solve2 (test args) input
  result
