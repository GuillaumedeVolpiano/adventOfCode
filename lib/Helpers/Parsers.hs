{-# LANGUAGE FlexibleContexts #-}

module Helpers.Parsers
  ( alnum
  , alpha
  , alphaNum
  , arrayFromString
  , characters
  , complexParser
  , custom
  , doubles
  , digitArrayFromString
  , integers
  , make2DArray
  , numbers
  , nums
  , splitOnSpace
  ) where

import           Data.Array.IArray  (IArray)
import           Data.Array.Unboxed (UArray, array)
import           Data.Char          (digitToInt)
import           Linear.V2          (V2 (..))
import           Text.Regex.TDFA    (getAllTextMatches, (=~))

alnum = "[[:alnum:]]+"

alpha = "[[:alpha:]]+"

nums = "-?[[:digit:]]+"

regexList :: String -> String -> [String]
regexList pat line = getAllTextMatches (line =~ pat)

alphaNum :: String -> [[String]]
alphaNum = custom alnum

characters :: String -> [[String]]
characters = custom alpha

custom :: String -> String -> [[String]]
custom pat = map (regexList pat) . lines

doubles :: String -> [[Double]]
doubles = map (map read) . custom nums

integers :: String -> [[Int]]
integers = map (map read) . custom nums

numbers :: (Num a, Read a) => String -> [[a]]
numbers = map (map read) . custom nums

-- | The 'complex parser' function parses a string with complex patterns. It
-- takes as arguments a list of splitter patterns, a list of parsing patterns
-- and a string, potentially unlined, and return a list of list of lists of
-- strings, taken from the original string, split around the splitters and
-- parsed with the patterns.
complexParser :: [String] -> [String] -> String -> [[[String]]]
complexParser splitters pats = 
  map (zipWith regexList pats . splitOnSplitters splitters) . lines

make2DArray :: IArray UArray a => [[a]] -> UArray (V2 Int) a
make2DArray l =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, l !! y !! x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = length (head l) - 1
    height = length l - 1

arrayFromString :: String -> UArray (V2 Int) Char
arrayFromString = make2DArray . lines

digitArrayFromString :: String -> UArray (V2 Int) Int
digitArrayFromString = make2DArray . map (map digitToInt) . lines

splitOnSpace :: String -> [[String]]
splitOnSpace = map (regexList "[^[:space:]]+") . lines

splitOnSplitters :: [String] -> String -> [String]
splitOnSplitters [] string = [string]
splitOnSplitters (s:ss) string = before : splitOnSplitters ss after
  where
    (before, _, after) = string =~ s :: (String, String, String)
