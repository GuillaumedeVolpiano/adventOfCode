{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Helpers.Runner
-- Description : The command line program shared by every year.
--
-- A year's Main is reduced to @main = runYear 2026 days@.
module Helpers.Runner
  ( runYear
  ) where

import           Control.DeepSeq                 (force)
import           Control.Exception               (evaluate)
import           Data.IntMap                     (IntMap)
import qualified Data.IntMap                     as IM (lookup)
import           Data.Time.Calendar              (toGregorian)
import           Data.Time.Clock                 (getCurrentTime, utctDay)
import           Helpers.Day                     (Day (Day))
import           Helpers.General.Streamly        (customPreciseTimeIt,
                                                  readInput, wallTimeIt)
import           System.Console.CmdArgs.Implicit (Data, cmdArgs, def, help, opt,
                                                  (&=))

data Arguments = Arguments
  { day      :: Int
  , test     :: Bool
  , wallTime :: Bool
  } deriving (Show, Data)

runYear :: Int -> IntMap Day -> IO ()
runYear year days = do
  time <- getCurrentTime
  let (_, _, curDay) = toGregorian . utctDay $ time
      arguments =
        Arguments
          { day =
              def
                &= help "Which day to process. Defaults to the current day"
                &= opt curDay
          , test = def &= help "Run on the example input (test/<year>/dayN.txt)"
          , wallTime = def &= help "Report wall Time rather than CPU time"
          }
  args <- cmdArgs arguments
  let theDay =
        case day args of
          0 -> curDay
          d -> d
      timer :: Show a => String -> Int -> IO a -> IO a
      timer =
        if wallTime args
          then wallTimeIt
          else customPreciseTimeIt
  case IM.lookup theDay days of
    Nothing -> fail $ "Day " ++ show theDay ++ " is not in the table of " ++ show year
    Just (Day solve1 _ solve2 _) -> do
      -- The input is read whole before the timers start.
      input <- readInput year theDay (test args)
      -- The answer is forced inside the timer, so that the time covers the
      -- whole computation and not just the building of a thunk.
      timer "Part 1." 4 (solve1 (test args) input >>= evaluate . force) >>= print
      timer "Part 2." 4 (solve2 (test args) input >>= evaluate . force) >>= print
