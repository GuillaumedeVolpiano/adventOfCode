module Day3
  ( part1
  , part2
  ) where

import           Data.List            (transpose)
import           Data.List.Split      (chunksOf)
import           Data.Text            (Text)
import           Helpers.Parsers.Text (signedInts)

isTriangle :: [Int] -> Bool
isTriangle [a, b, c] = a + b > c && a + c > b && b + c > a

part1 :: Bool -> Text -> String
part1 _ = show . length . filter isTriangle . signedInts

part2 :: Bool -> Text -> String
part2 _ =
  show . length . filter isTriangle . chunksOf 3 . concat . transpose . signedInts
