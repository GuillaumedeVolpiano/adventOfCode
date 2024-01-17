module Day1
  ( part1
  , part2
  ) where

import           Data.IntSet     (IntSet, empty, insert, member)
import           Data.Sequence   (Seq ((:<|), (:|>)), fromList)

findRepeat :: IntSet -> Int -> Seq Int -> Int
findRepeat seen cur (val :<| rest)
  | cur `member` seen = cur
  | otherwise = findRepeat (insert cur seen) (cur + val) (rest :|> val)

part1 :: Bool -> String -> String
part1 _ = show . sum . map (read . filter (/= '+')) . lines

part2 :: Bool -> String -> String
part2 _ =
  show . findRepeat empty 0 . fromList . map (read . filter (/= '+')) . lines
