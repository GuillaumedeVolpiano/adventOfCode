module Day16
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed   (UArray, array, assocs, (!))
import           Data.Hashable        (Hashable, hashWithSalt)
import           Data.Map             as M (lookup)
import           Data.Maybe           (mapMaybe)
import           Data.Text            (Text)
import           Helpers.Graph        (Pos, dirs, east, left, right)
import           Helpers.Parsers.Text (arrayFromText)
import           Helpers.Search       (dijkstraGoal)

type Maze = UArray Pos Char

data Reindeer = Reindeer
  { pos :: Pos
  , dir :: Dir
  } deriving (Show, Eq, Ord)

type Dir = Pos

instance Hashable Reindeer where
  hashWithSalt s (Reindeer p d) = hashWithSalt s (p, d)

dijkstra :: Text -> Int
dijkstra input =
  minimum . mapMaybe (flip M.lookup dijk . Reindeer goalPos) $ dirs
  where
    dijk = fst . dijkstraGoal start 0 neighbours $ ((== goalPos) . pos)
    maze = arrayFromText input
    startPos = fst . head . filter ((== 'S') . snd) . assocs $ maze
    goalPos = fst . head . filter ((== 'E') . snd) . assocs $ maze
    start = Reindeer startPos east
    neighbours reindeer
      | maze ! (pos reindeer + dir reindeer) == '#' = turn reindeer
      | otherwise =
        (reindeer {pos = pos reindeer + dir reindeer}, 1) : turn reindeer
    turn reindeer =
      [ (reindeer {dir = left . dir $ reindeer}, 1000)
      , (reindeer {dir = right . dir $ reindeer}, 1000)
      ]

part1 :: Bool -> Text -> String
part1 _ = show . dijkstra

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
