module Day2
  ( part1
  , part2
  ) where

import           Intcode (initialise, runIntcode, update)

goal = 19690720

findNounVerb :: String -> Int
findNounVerb inst =
  (\(a, b) -> 100 * a + b) . head . filter ((== goal) . runIntcode . nounVerb) $
  [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]
  where
    nounVerb (a, b) = update 1 a . update 2 b . initialise $ inst

part1 :: Bool -> String -> String
part1 test
  | test = show . runIntcode . initialise
  | otherwise = show . runIntcode . update 1 12 . update 2 2 . initialise

part2 :: Bool -> String -> String
part2 test
  | test = const "testing"
  | otherwise = show . findNounVerb
