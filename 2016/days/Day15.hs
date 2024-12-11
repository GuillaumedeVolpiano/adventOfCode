module Day15
  ( part1
  , part2
  ) where

import           Data.Text            (Text)
import           Helpers.Parsers.Text (signedInts)

data Disc =
  Disc Index Positions Start
  deriving (Show, Eq, Ord)

type Index = Int

type Positions = Int

type Start = Int

canFallThrough :: Int -> Disc -> Bool
canFallThrough t (Disc a b c) = (c + t + a) `mod` b == 0

makeDisc :: [Int] -> Disc
makeDisc [a, b, _, c] = Disc a b c

part1 :: Bool -> Text -> String
part1 _ input =
  show . head . filter (\x -> all (canFallThrough x) discs) $ [0 ..]
  where
    discs = map makeDisc . signedInts $ input

part2 :: Bool -> Text -> String
part2 _ input =
  show . head . filter (\x -> all (canFallThrough x) discs) $ [0 ..]
  where
    discs = (Disc 7 11 0 :) . map makeDisc . signedInts $ input
