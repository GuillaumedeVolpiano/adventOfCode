{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import           Data.Time.Calendar              (toGregorian)
import           Data.Time.Clock                 (getCurrentTime, utctDay)
import           General                         (customPreciseTimeIt,
                                                  retrieveInput)
import           System.Console.CmdArgs.Implicit (Data, Typeable, cmdArgs, def,
                                                  help, opt, (&=))

data Arguments =
  Arguments
    { year :: Int
    , day  :: Int
    , test :: Bool
    }
  deriving (Show, Data, Typeable)

main :: IO ()
main = do
  time <- getCurrentTime
  let (curYear, _, curDay) = toGregorian . utctDay $ time
      arguments =
        Arguments
          { year =
              def &= help "Which year to process. Defaults to the current year" &=
              opt (fromInteger curYear :: Int)
          , day =
              def &= help "Which day to process. Defaults to the current day" &=
              opt curDay
          , test = def &= help "Run the test suite? Defaults to True if used"
          }
  args <- cmdArgs arguments
  let theYear =
        case year args of
          0 -> fromInteger curYear
          _ -> year args
      theDay =
        case day args of
          0 -> curDay
          _ -> day args
  input <- retrieveInput theYear theDay $ test args
  print input
  customPreciseTimeIt "Part 1. CPU Time" 4 $ print "Part 1"
  customPreciseTimeIt "Part 2. CPU Time" 4 $ print "Part 2"
