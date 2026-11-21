{-# LANGUAGE ExistentialQuantification #-}
-- |
-- Module      : Helpers.Day
-- Description : What a day of a year provides to the runner, the tests and
--               the benchmarks.
module Helpers.Day
  ( Solver
  , Day (Day)
  ) where

import           Control.DeepSeq      (NFData)
import           Helpers.Input   (Input)

-- | A solver is given True when it runs on the example input, and the
-- whole input: see "Helpers.Input" for the ways to read it.
type Solver a = Bool -> Input -> IO a

-- | Both parts of a day, each with the answer to the example given in the
-- puzzle text (Nothing until it is known). Each part may return its own
-- type.
data Day =
  forall a b. (Eq a, Show a, NFData a, Eq b, Show b, NFData b) =>
              Day (Solver a) (Maybe a) (Solver b) (Maybe b)
