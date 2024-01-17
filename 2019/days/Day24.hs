module Day24
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, indices, (!))
import           Data.Bits          (bit, clearBit, popCount, setBit, testBit,
                                     (.&.), (.|.))
import           Data.IntSet        (IntSet, empty, fromList, insert, member,
                                     toList, unions)
import           Data.Maybe         (Maybe (Just, Nothing), mapMaybe)
import           Helpers.Graph      (Pos)
import           Helpers.Parsers    (boolArrayFromString)

type Bugs = Int

type RecurBugs = [Bugs]

birth :: String -> Bugs
birth = foldr bitify 0
  where
    bitify a b
      | a == '#' = 1 + 2 * b
      | a == '.' = 2 * b
      | a == '\n' = b

recurGol :: RecurBugs -> RecurBugs
recurGol rBugs
  | recGol 0 0 (head rBugs) == 0 = rGol (0 : rBugs)
  | otherwise = recGol 0 0 (head rBugs) : rGol (0 : rBugs)

recGol :: Bugs -> Bugs -> Bugs -> Bugs
recGol a b c = foldr (status a b c) b $ [0 .. 11] ++ [13 .. 24]

rGol :: RecurBugs -> RecurBugs
rGol [a]
  | recGol a 0 0 == 0 = []
  | otherwise = [recGol a 0 0]
rGol [a, b] = recGol a b 0 : rGol [b]
rGol (a:b:c:xs) = recGol a b c : rGol (b : c : xs)

status :: Bugs -> Bugs -> Bugs -> Int -> Bugs -> Bugs
status a b c n v
  | (b `testBit` n && bugMask a b c n == 1) ||
      (not (b `testBit` n) && bugMask a b c n `elem` [1, 2]) = v `setBit` n
  | otherwise = v `clearBit` n
  where
    bugMask a b c n = left a b c n + right a b c n + up a b c n + down a b c n
    left a b c n
      | n `mod` 5 == 0 = bitify a 11
      | n == 13 = sum . map (bitify c) $ [4, 9, 14, 19, 24]
      | otherwise = popCount $ bit (n - 1) .&. b
    right a b c n
      | n `mod` 5 == 4 = bitify a 13
      | n == 11 = sum . map (bitify c) $ [0, 5, 10, 15, 20]
      | otherwise = popCount $ bit (n + 1) .&. b
    up a b c n
      | n < 5 = bitify a 7
      | n == 17 = sum . map (bitify c) $ [20, 21, 22, 23, 24]
      | otherwise = popCount $ bit (n - 5) .&. b
    down a b c n
      | n > 19 = bitify a 17
      | n == 7 = sum . map (bitify c) $ [0, 1, 2, 3, 4]
      | otherwise = popCount $ bit (n + 5) .&. b
    bitify bugLayer val = popCount $ bugLayer .&. bit val

gol :: Bugs -> Bugs
gol bugs = foldr status bugs [0 .. 24]
  where
    status n v
      | (bugs `testBit` n && (popCount . bugMask $ n) == 1) ||
          (not (bugs `testBit` n) && (popCount . bugMask $ n) `elem` [1, 2]) =
        v `setBit` n
      | otherwise = v `clearBit` n
    bugMask = (.&.) bugs . foldr ((.|.) . bit) 0 . neighbours
    neighbours = left <> right <> up <> down
    left n
      | n `mod` 5 == 0 = []
      | otherwise = [n - 1]
    right n
      | n `mod` 5 == 4 = []
      | otherwise = [n + 1]
    up n
      | n < 5 = []
      | otherwise = [n - 5]
    down n
      | n > 19 = []
      | otherwise = [n + 5]

biodiversity :: IntSet -> Bugs -> Bugs
biodiversity seen bugs
  | bugs `member` seen = bugs
  | otherwise = biodiversity (insert bugs seen) $ gol bugs

part1 :: Bool -> String -> String
part1 _ = show . biodiversity empty . birth

part2 :: Bool -> String -> String
part2 test =
  show . sum . map popCount . (!! time) . iterate recurGol . (: []) . birth
  where
    time
      | test = 10
      | otherwise = 200
