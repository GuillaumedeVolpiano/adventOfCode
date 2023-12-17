module Day8
  ( part1
  , part2
  ) where

import           Data.Maybe      (fromJust)
import           Data.Set        as St (Set, fromList, null, size, union, (\\))
import           Helpers.Parsers (characters)

import           Debug.Trace

key :: [Set Char] -> [(Set Char, Char)]
key s =
  [ (zero, '0')
  , (one, '1')
  , (two, '2')
  , (three, '3')
  , (four, '4')
  , (five, '5')
  , (six, '6')
  , (seven, '7')
  , (eight, '8')
  , (nine, '9')
  ]
  where
    one = head . filter ((== 2) . size) $ s
    four = head . filter ((== 4) . size) $ s
    seven = head . filter ((== 3) . size) $ s
    eight = head . filter ((== 7) . size) $ s
    zeroSixNine = filter ((== 6) . size) s
    fourSeven = four `union` seven
    nine = head . filter (St.null . (fourSeven \\)) $ zeroSixNine
    zeroSix = filter (not . St.null . (fourSeven \\)) zeroSixNine
    zero = head . filter (St.null . (one \\)) $ s
    six = head . filter (not. St.null . (one \\) ) $ s
    twoThreeFive = filter ((== 5) . size) s
    two = head . filter ((== 2) . size . (fourSeven \\)) $ twoThreeFive
    threeFive =
      filter ((== 1) . size . (fourSeven \\)) twoThreeFive
    three = head . filter (St.null . (one \\)) $ threeFive
    five = head . filter (not . St.null . (one \\)) $ threeFive

decode :: [Set Char] -> Int
decode string =
  trace (show display ++ "\n" ++ show (key mapping) ++ "\n\n") read .
  map (\t -> fromJust . lookup t $ key mapping) $
  display
  where
    (mapping, display) = splitAt 10 string

part1 :: Bool -> String -> String
part1 _ =
  show .
  sum .
  map (length . filter (\t -> length t `elem` [2, 3, 4, 7]) . drop 10) .
  characters

part2 :: Bool -> String -> String
part2 _ = show . map (decode . map fromList) . characters
