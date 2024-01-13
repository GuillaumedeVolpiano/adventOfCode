module Day16
  ( part1
  , part2
  ) where

import           Data.Char   (digitToInt, intToDigit)
import           Data.List   (foldl')

import           Debug.Trace

buildPattern :: Int -> [Int]
buildPattern n = tail . cycle . concatMap (replicate n) $ [0, 1, 0, -1]

phase :: [Int] -> [Int]
phase list =
  map (abs . flip rem 10 . sum . zipWith (*) list . buildPattern) .
  take (length list) $
  [1 ..]

expandAndOffset :: [Int] -> [Int]
expandAndOffset list = complemented ++ batched
  where
    offset = foldl' (\a b -> b + 10 * a) 0 . take 7 $ list
    l = length list
    remain = l * 10000 - offset
    complement = mod remain l
    batchLength = remain - complement
    batched = take batchLength . cycle $ list
    complemented = drop (l - complement) list

longPhase :: [Int] -> [Int]
longPhase = scanr1 (\a b -> (a + b) `rem` 10)

part1 :: Bool -> String -> String
part1 _ =
  map intToDigit .
  take 8 . last . take 101 . iterate phase . map digitToInt . init

part2 :: Bool -> String -> String
part2 _ =
  map intToDigit .
  take 8 .
  last . take 101 . iterate longPhase . expandAndOffset . map digitToInt . init
