module Day5
  ( part1
  , part2
  ) where

import           Data.Char (ord, toUpper)

react :: String -> String -> Int
react a b
  | null b = length a
  | null a = react [head b] $ tail b
  | abs (ord (head a) - ord (head b)) == 32 = react (tail a) $ tail b
  | otherwise = react (head b : a) $ tail b

reactPruned :: String -> Int
reactPruned polymer =
  minimum . map (\p -> react "" . filter (`notElem` p) $ polymer) $
  [[x, toUpper x] | x <- ['a' .. 'z']]

part1 :: Bool -> String -> String
part1 _ = show . react "" . filter (/= '\n')

part2 :: Bool -> String -> String
part2 _ = show . reactPruned . filter (/= '\n')
