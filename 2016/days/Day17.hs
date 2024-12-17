module Day17
  ( part1
  , part2
  ) where

import           Data.Text     as T (Text, init, unpack)
import           Helpers.Graph (Pos, east, north, south, west)
import           MD5           (md5Concat)

data Search =
  Search Pos Path
  deriving (Show, Eq, Ord)

type Path = String

part1 :: Bool -> Text -> String
part1 _ = unpack . T.init

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
