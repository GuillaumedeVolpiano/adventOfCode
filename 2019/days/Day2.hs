module Day2
  ( part1
  , part2
  ) where

import           Intcode (evalIntcode, initialise, update)

goal :: Int
goal = 19690720

findNounVerb :: String -> Int
findNounVerb inst =
  (\(a, b) -> 100 * a + b) . head . filter ((== goal) . evalIntcode . nounVerb) $
  [(x, y) | x <- [0 .. 99], y <- [0 .. 99]]
  where
    nounVerb (a, b) = update 1 a . update 2 b . initialise $ inst

restoreGravityAssist :: Bool -> String -> Int
restoreGravityAssist test
  | test = evalIntcode . initialise
  | otherwise = evalIntcode . update 1 12 . update 2 2 . initialise

part1 :: Bool -> String -> String
part1 test = show . restoreGravityAssist test

part2 :: Bool -> String -> String
part2 test
  | test = const "testing"
  | otherwise = show . findNounVerb
