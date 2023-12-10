module Helpers.Parsers
  ( alnum
  , alphaNum
  , arrayFromString
  , characters
  , complexParser
  , doubles
  , integers
  , numbers
  , splitOnSpace
  ) where

import           Text.Regex.TDFA (getAllTextMatches, (=~))
import Linear.V2(V2(..))
import Data.Array.Unboxed (UArray, array)

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

arrayFromString :: String -> UArray (V2 Int) Char
arrayFromString s = array (V2 0 0, V2 width height) [(V2 x y, l !! y !! x) | x <- [0..width], y <- [0..height]]
    where
            l = lines s
            width = length (head l) - 1
            height = length l - 1

characters :: String -> [[String]]
characters = map (regexList alpha) . lines

splitOnSpace :: String -> [[String]]
splitOnSpace = map (regexList "[^[:space:]]+") . lines

splitOnSplitters :: [String] -> String -> [String]
splitOnSplitters [] string = [string]
splitOnSplitters (s:ss) string = before : splitOnSplitters ss after
  where
    (before, _, after) = string =~ s :: (String, String, String)
