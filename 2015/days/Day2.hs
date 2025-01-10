module Day2
  ( part1
  , part2
  ) where

import           Data.Text            (Text)
import           Helpers.Parsers.Text (signedInts)

wrapping :: [Int] -> Int
wrapping [w, l, h] = 2 * sum sides + minimum sides
  where
    sides = [w * l, w * h, l * h]

ribbon :: [Int] -> Int
ribbon [w, l, h] = 2 * minimum sides + w * l * h
  where
    sides = [w + l, w + h, l + h]

part1 :: Bool -> Text -> String
part1 _ = show . sum . map wrapping . signedInts

part2 :: Bool -> Text -> String
part2 _ = show . sum . map ribbon . signedInts
