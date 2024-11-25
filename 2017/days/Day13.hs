module Day13
  ( part1
  , part2
  ) where

import           Data.Maybe      (mapMaybe)
import           Helpers.Parsers (numbers)

data Scanner =
  Scanner Depth Range
  deriving (Show)

type Depth = Int

type Range = Int

inRange :: Int -> Scanner -> Bool
inRange start (Scanner depth range) =
  (start + depth) `mod` (2 * (range - 1)) == 0

severity :: Scanner -> Int
severity (Scanner depth range) = depth * range

mkScanners :: String -> [Scanner]
mkScanners = map (\[depth, range] -> Scanner depth range) . numbers

check :: [Scanner] -> Int -> Maybe Int
check scanners start
  | any (inRange start) scanners = Nothing
  | otherwise = Just start

part1 :: Bool -> String -> String
part1 _ =
  show
    . sum
    . map severity
    . filter (inRange 0)
    . map (\[depth, range] -> Scanner depth range)
    . numbers

part2 :: Bool -> String -> String
part2 _ input = show . head . mapMaybe (check scanners) $ [0 ..]
  where
    scanners = mkScanners input
