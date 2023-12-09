module Day9
  ( part1
  , part2
  ) where

import           Data.Sequence   as Sq (Seq ((:<|), (:|>)), empty, fromList,
                                        null, singleton)
import           Text.Regex.TDFA (getAllTextMatches, (=~))

parseLine :: String -> [Seq Int]
parseLine =
  map (fromList . map read . (\t -> getAllTextMatches (t =~ "-?[-0-9]+"))) .
  lines

toZero :: Seq Int -> Seq (Seq Int)
toZero s
  | foldl (\a b -> a && b == 0) True s = empty
  | otherwise = s :<| (toZero . diffs $ s)

diffs :: Seq Int -> Seq Int
diffs (a :<| xs@(b :<| r))
  | Sq.null r = singleton (b - a)
  | otherwise = (b - a) :<| diffs xs

extrapolate :: Seq (Seq Int) -> Int
extrapolate = foldl (\a (_ :|> b) -> a + b) 0

extrapolateBackwards :: Seq (Seq Int) -> Int
extrapolateBackwards = foldr (\(b :<| _) a -> b - a) 0

part1 :: Bool -> String -> String
part1 _ = show . sum . map (extrapolate . toZero) . parseLine

part2 :: Bool -> String -> String
part2 _ = show . sum . map (extrapolateBackwards . toZero) . parseLine
