module Day6
  ( part1
  , part2
  ) where

import           Data.List       as L (length)
import           Data.Sequence   as Sq (Seq ((:<|), (:|>)), fromList, length)
import           Helpers.Parsers (integers)

grow :: Seq Int -> Seq Int
grow f@(h :<| r@(o :|> s :|> e))
  | Sq.length f == 7 = r :|> h :|> 0 :|> h
  | otherwise = o :|> h + s :|> e :|> h

part1 :: Bool -> String -> String
part1 _ input = show . sum . last . take 81 . iterate grow $ fish
  where
    ages = concat . integers $ input
    fish = fromList . map (\x -> L.length . filter (== x) $ ages) $ [0 .. 6]

part2 :: Bool -> String -> String
part2 _ input = show . sum . last . take 257 . iterate grow $ fish
  where
    ages = concat . integers $ input
    fish = fromList . map (\x -> L.length . filter (== x) $ ages) $ [0 .. 6]
