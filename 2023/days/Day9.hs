module Day9
  ( part1
  , part2
  ) where

import           Data.ByteString            (ByteString)
import           Helpers.Parsers.ByteString (signedInts)
import Data.List (uncons)
import Data.Maybe (maybe)

toZero :: [Int] -> [[Int]]
toZero s
  | all (== 0) s = []
  | otherwise = s : (toZero . diffs $ s)

diffs :: [Int] -> [Int]
diffs (a:xs@(b:r))
  | null r = [b - a]
  | otherwise = (b - a) : diffs xs

extrapolate :: [[Int]] -> Int
extrapolate = sum . map last

extrapolateBackwards :: [[Int]] -> Int
extrapolateBackwards = foldr ((-) . maybe (error "empty list") fst . uncons) 0

part1 :: Bool -> ByteString -> String
part1 _ = show . sum . map (extrapolate . toZero) . signedInts

part2 :: Bool -> ByteString -> String
part2 _ = show . sum . map (extrapolateBackwards . toZero) . signedInts
