module Day2
  ( part1
  , part2
  ) where

import           Data.List     as L (zipWith, foldl')
import           Data.Map      (Map, fromList, member, (!))
import           Helpers.Graph (Pos, east, north, south, west)
import           Linear.V2     (V2 (..))
import           Data.Text     as T (Text, lines, foldl')

type Dial = Map Pos Key

type Key = Char

(start1, dial1) = makeDial ["123", "456", "789"]

(start2, dial2) = makeDial ["  1", " 234", "56789", " ABC", "  D"]

makeDial :: [String] -> (Pos, Dial)
makeDial d = (start, fromList dial)
  where
    dial =
      concatMap (filter ((/= ' ') . snd))
        . zipWith (\y r -> zipWith (\x v -> (V2 x y, v)) [0 ..] r) [0 ..]
        $ d
    start = fst . head . filter ((== '5') . snd) $ dial

findKey :: Dial -> (Pos, [Key]) -> Text -> (Pos, [Key])
findKey dial (pos, keys) line = (pos', key : keys)
  where
    pos' = T.foldl' (move dial) pos line
    key = dial ! pos'

move :: Dial -> Pos -> Char -> Pos
move dial pos c
  | pos' `member` dial = pos'
  | otherwise = pos
  where
    movement
      | c == 'U' = north
      | c == 'D' = south
      | c == 'L' = west
      | c == 'R' = east
    pos' = pos + movement

part1 :: Bool -> Text -> String
part1 _ = reverse . snd . L.foldl' (findKey dial1) (start1, []) . T.lines

part2 :: Bool -> Text -> String
part2 _ = reverse . snd . L.foldl' (findKey dial2) (start2, []) . T.lines
