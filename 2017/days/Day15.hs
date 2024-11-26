module Day15
  ( part1
  , part2
  ) where

import           Data.Bits       ((.&.))
import           Helpers.Parsers (numbers)

genA :: Int -> [Int]
genA = gen 16807

genB :: Int -> [Int]
genB = gen 48271

gen :: Int -> Int -> [Int]
gen factor = tail . iterate op
  where
    op = flip mod 2147483647 . (factor *)

fstGen :: [Int] -> [(Int, Int)]
fstGen [a, b] = zip (genA a) (genB b)

sndGen :: [Int] -> [(Int, Int)]
sndGen [a, b] = zip (filter (mult 4) . genA $ a) (filter (mult 8) . genB $ b)
  where
    mult v x = x `mod` v == 0

score :: Int -> [(Int, Int)] -> Int
score numPairs = length . filter agree . take numPairs

agree :: (Int, Int) -> Bool
agree (a, b) = a .&. 0xffff == b .&. 0xffff

part1 :: Bool -> String -> String
part1 _ = show . score (4 * 10 ^ 7) . fstGen . concat . numbers

part2 :: Bool -> String -> String
part2 _ = show . score (5 * 10 ^ 6) . sndGen . concat . numbers
