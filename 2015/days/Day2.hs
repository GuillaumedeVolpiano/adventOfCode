module Day2
  ( part1
  , part2
  ) where

import           Data.ByteString            (ByteString)
import           Helpers.Parsers.ByteString (signedInts)

wrapping :: [Int] -> Int
wrapping [w, l, h] = 2 * sum sides + minimum sides
  where
    sides = [w * l, w * h, l * h]

ribbon :: [Int] -> Int
ribbon [w, l, h] = 2 * minimum sides + w * l * h
  where
    sides = [w + l, w + h, l + h]

part1 :: Bool -> ByteString -> String
part1 _ = show . sum . map wrapping . signedInts

part2 :: Bool -> ByteString -> String
part2 _ = show . sum . map ribbon . signedInts
