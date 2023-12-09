module Parsers
  ( alnum
  , alphaNum
  , characters
  , complexParser
  , doubles
  , integers
  , numbers
  , splitOnSpace
  ) where

import           Text.Regex.TDFA (getAllTextMatches, (=~))

numbers = "-?[[:digit:]]+"

alnum = "[[:alnum:]]+"

alpha = "[[:alpha:]]+"

regexList :: String -> String -> [String]
regexList pat line = getAllTextMatches (line =~ pat)

integers :: String -> [[Int]]
integers = map (map read . regexList numbers) . lines

-- | The 'complex parser' function parses a string with complex patterns. It
-- takes as arguments a list of splitter patterns, a list of parsing patterns
-- and a string, potentially unlined, and return a list of list of lists of
-- strings, taken from the original string, split around the splitters and
-- parsed with the patterns.
complexParser :: [String] -> [String] -> String -> [[[String]]]
complexParser splitters pats =
  map (zipWith regexList pats . splitOnSplitters splitters) . lines

doubles :: String -> [[Double]]
doubles = map (map read . regexList numbers) . lines

alphaNum :: String -> [[String]]
alphaNum = map (regexList alnum) . lines

characters :: String -> [[String]]
characters = map (regexList alpha) . lines

splitOnSpace :: String -> [[String]]
splitOnSpace = map (regexList "[^[:space:]]+") . lines

splitOnSplitters :: [String] -> String -> [String]
splitOnSplitters [] string = [string]
splitOnSplitters (s:ss) string = before : splitOnSplitters ss after
  where
    (before, _, after) = string =~ s :: (String, String, String)
