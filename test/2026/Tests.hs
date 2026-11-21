module Main where

import           Days            (days)
import           Helpers.Testing (testYear)

-- The expected answers are filled in in 2026/days/Days.hs.
main :: IO ()
main = testYear 2026 days
