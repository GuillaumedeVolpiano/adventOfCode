module Day5
  ( part1
  , part2
  ) where

import           Helpers.Parsers (integers)

import           Data.List       (group, sort, tails)
import           Linear.V2

type Pos = V2 Int

type Line = (Pos, Pos)

equation :: Line -> [Pos]
equation (V2 a b, V2 c d)
  | a == c = [V2 a y | y <- ly]
  | b == d = [V2 x b | x <- lx]
  | a > c && b > d || c > a && d > b =
    [V2 x y | x <- lx, y <- ly, y == x + b - a]
  | otherwise = [V2 x y | x <- lx, y <- ly, y + x == a + b]
  where
    lx = [min a c .. max a c]
    ly = [min b d .. max b d]

isVertical :: Line -> Bool
isVertical (V2 _ x, V2 _ y) = x == y

isHorizontal :: Line -> Bool
isHorizontal (V2 x _, V2 y _) = x == y

part1 :: Bool -> String -> String
part1 _ input =
  show .
  length . filter (\t -> length t > 1) . group . sort . concatMap equation $
  hv
  where
    vents = map (\[a, b, c, d] -> (V2 a b, V2 c d)) . integers $ input
    h = filter isHorizontal vents
    v = filter isVertical vents
    hv = h ++ v

part2 :: Bool -> String -> String
part2 _ input =
  show .
  length . filter (\t -> length t > 1) . group . sort . concatMap equation $
  vents
  where
    vents = map (\[a, b, c, d] -> (V2 a b, V2 c d)) . integers $ input
