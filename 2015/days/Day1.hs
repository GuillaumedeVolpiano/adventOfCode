module Day1
  ( part1
  , part2
  ) where

import           Data.List            (elemIndex)
import           Data.Maybe           (fromJust)
import           Data.Text            (Text, unpack)
import           Helpers.Parsers.Text

calc :: Char -> Int -> Int
calc ')'  = (+ (-1))
calc '('  = (+ 1)
calc '\n' = id

part1 :: Bool -> Text -> String
part1 _ = show . foldr calc 0 . unpack

part2 :: Bool -> Text -> String
part2 _ = show . fromJust . elemIndex (-1) . scanl (flip calc) 0 . unpack
