module Day12
  ( part1
  , part2
  ) where

import           Data.Bifunctor  (first, second)
import           Data.List       (groupBy, sortBy)
import           Data.Maybe      (Maybe (Just, Nothing), isNothing, mapMaybe)
import           Data.Set        (Set, fromList, size)
import           Helpers.Parsers (custom, integers)

import           Debug.Trace

extractPatterns :: ([String], [Int]) -> Int
extractPatterns (ss, c)
  | null c && any ('#' `elem`) ss = 0
  | null ss && not (null c) = 0
  | null ss || null c = 1
  | isNothing rawResult = 0
  | otherwise = sum . map (\(a, b) -> b * extractPatterns (xs, a)) $ grouped
  where
    (s:xs) = ss
    rawResult = extractPatternsMech (s, c)
    (Just result) = rawResult
    grouped =
      map (second length) .
      foldl grouping [] . sortBy (\(_, a) (_, b) -> compare a b) $
      result
    grouping [] (a, b) = [(b, [a])]
    grouping l@((a, b):ls) (c, d)
      | a == d = (a, c : b) : ls
      | otherwise = (d, [c]) : l

extractPatternsMech :: (String, [Int]) -> Maybe [([Int], [Int])]
extractPatternsMech (s, c)
  | null c && '#' `elem` s = Nothing
  | null s || null c = Just [([], c)]
  | length s < b && '#' `elem` s = Nothing
  | length s < b = Just [([], c)]
  | a == '#' && length s == b = Just [([b], bs)]
  | length s == b && '#' `elem` s = Just [([b], bs)]
  | length s == b = Just [([b], bs), ([], c)]
  | a == '#' && head postPat == '#' = Nothing
  | a == '#' = insertRes $ extractPatternsMech (tail postPat, bs)
  | head postPat == '#' = extractPatternsMech (as, c)
  | otherwise =
    combine
      (insertRes $ extractPatternsMech (tail postPat, bs))
      (extractPatternsMech (as, c))
  where
    insertRes Nothing  = Nothing
    insertRes (Just t) = Just (map (first (b :)) t)
    combine Nothing t         = t
    combine t Nothing         = t
    combine (Just x) (Just y) = Just (x ++ y)
    (a:as) = s
    (b:bs) = c
    (curPat, postPat) = splitAt b s

part1 :: Bool -> String -> String
part1 _ input = show . sum . map extractPatterns $ pairs
  where
    springs = custom "[?#]+" input
    records = integers input
    pairs = zip springs records

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
