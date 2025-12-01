{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Data.IntMap                     (IntMap, empty, fromList, (!))
import           Data.Time.Calendar              (toGregorian)
import           Data.Time.Clock                 (getCurrentTime, utctDay)
import           Data.Word                       (Word8)
import           Day1
import           Day10
import           Day11
import           Day12
import           Day2
import           Day3
import           Day4
import           Day5
import           Day6
import           Day7
import           Day8
import           Day9
import           Helpers.General.Streamly        (customPreciseTimeIt,
                                                  retrieveInput, wallTimeIt)
import           Streamly.Data.Stream            (Stream)
import           System.Console.CmdArgs.Implicit (Data, cmdArgs, def, help, opt,
                                                  (&=))

data Arguments = Arguments
  { day         :: Int
  , test        :: Bool
  , proxy       :: Bool
  , wallTime    :: Bool
  , interactive :: Bool
  } deriving (Show, Data)

type Solver = (Bool -> Stream IO Word8 -> IO ())

type Day = (Solver, Solver)

solver :: IntMap Day
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
    ]

interSolver :: IntMap (Stream IO Word8 -> IO ())
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
  let year = 2025
      theDay =
        case day args of
          0 -> curDay
          _ -> day args
  input <- retrieveInput year theDay (test args) (proxy args)
  let (solve1, solve2) = solver ! theDay
      timer =
        if wallTime args
          then wallTimeIt
          else customPreciseTimeIt
      result
        | interactive args = interSolver ! theDay $ input
        | otherwise = do
          timer "Part 1." 4 . solve1 (test args) $ input
          timer "Part 2." 4 . solve2 (test args) $ input
  result
