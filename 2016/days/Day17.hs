module Day17
  ( part1
  , part2
  ) where

import           Data.Text as T (Text, init, unpack)
import           MD5       (md5Concat)

part1 :: Bool -> Text -> String
part1 _ = unpack . T.init

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
