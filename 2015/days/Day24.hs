module Day24
  ( part1
  , part2
  ) where

import           Data.ByteString           (ByteString)
import           Data.List                 (sortBy, subsequences)
import           Data.Ord                  (comparing)
import           FlatParse.Basic           (anyAsciiDecimalInt, runParser, skip,
                                            some)
import           Helpers.Parsers.FlatParse (Parser, extract)

subsequencesOfLength :: Int -> [a] -> [[a]]
subsequencesOfLength 0 _ = []
subsequencesOfLength l xs@(x:xs')
  | length xs == l = [xs]
  | otherwise =
    map (x :) (subsequencesOfLength (l - 1) xs') ++ subsequencesOfLength l xs'

divideIn :: Int -> [Int] -> Int
divideIn x packages = minimum . map product . head $ subsequences
  where
    partSize = flip div x . sum $ packages
    subsequences =
      dropWhile null
        . map
            (filter ((== partSize) . sum) . flip subsequencesOfLength packages)
        $ [1 ..]

parseInput :: Parser [Int]
parseInput = some (anyAsciiDecimalInt <* skip 1)

part1 :: Bool -> ByteString -> String
part1 _ = show . divideIn 3 . extract . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ = show . divideIn 4 . extract . runParser parseInput
