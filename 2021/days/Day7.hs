module Day7
  ( part1
  , part2
  ) where

import           Data.List       (sort)
import           Helpers.Parsers (integers)

fuelCostSimple :: Int -> [Int] -> Int
fuelCostSimple point = sum . map (\x -> abs (point - x))

fuelCostCumulative :: Int -> [Int] -> Int
fuelCostCumulative point = sum . map (\x -> arSum . abs $ (point - x))
  where
    arSum n = div (n * (n + 1)) 2

dichotomy :: [Int] -> (Int -> [Int] -> Int) -> Int -> Int -> Int
dichotomy crabs fuelFormula mini maxi
  | maxi - mini == 1 = min (fuelFormula maxi crabs) (fuelFormula mini crabs)
  | fuelFormula maxi crabs < fuelFormula mini crabs =
    dichotomy crabs fuelFormula midPoint maxi
  | fuelFormula mini crabs < fuelFormula maxi crabs =
    dichotomy crabs fuelFormula mini midPoint
  where
    midPoint = div (maxi + mini) 2

part1 :: Bool -> String -> String
part1 _ input =
  show . dichotomy crabs fuelCostSimple (minimum crabs) $ maximum crabs
  where
    crabs = concat . integers $ input

part2 :: Bool -> String -> String
part2 _ input =
  show . dichotomy crabs fuelCostCumulative (minimum crabs) $ maximum crabs
  where
    crabs = concat . integers $ input
