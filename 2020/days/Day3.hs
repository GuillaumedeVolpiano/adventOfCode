module Day3
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, bounds, (!))
import           Data.List          (unfoldr)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

type Toboggan = UArray Pos Char

findPos :: Int -> Pos -> Pos -> Maybe (Pos, Pos)
findPos int dp p@(V2 _ y)
  | y > int = Nothing
  | otherwise = Just (p, p + dp)

allTrees :: Pos -> Toboggan -> Int
allTrees dp toboggan =
  length .
  filter (== '#') . map (\x -> toboggan ! vMod x) . unfoldr (findPos my dp) $
  V2 0 0
  where
    vMod (V2 x y) = V2 (mod x (mx + 1)) y
    (_, V2 mx my) = bounds toboggan

part1 :: Bool -> String -> String
part1 _ = show . allTrees (V2 3 1) . arrayFromString

part2 :: Bool -> String -> String
part2 _ input =
  show . product . map (`allTrees` toboggan) $
  [V2 1 1, V2 3 1, V2 5 1, V2 7 1, V2 1 2]
  where
    toboggan = arrayFromString input
