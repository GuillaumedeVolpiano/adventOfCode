module Day24
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, indices, (!))
import           Data.Bits          (bit, clearBit, popCount, setBit, testBit,
                                     (.&.), (.|.))
import           Data.IntMap        as M (IntMap, alter, findWithDefault,
                                          insert, keys, map, singleton)
import           Data.IntSet        as St (IntSet, empty, fromList, insert,
                                           member, toList, unions)
import           Data.List          as L (map)
import           Data.Maybe         (Maybe (Just, Nothing), mapMaybe)
import           Helpers.Graph      (Pos)
import           Helpers.Parsers    (boolArrayFromString)

type Bugs = Int

type RecurBugs = IntMap Bugs

birth :: String -> Bugs
birth = foldr bitify 0
  where
    bitify a b
      | a == '#' = 1 + 2 * b
      | a == '.' = 2 * b
      | a == '\n' = b

recurGol :: RecurBugs -> RecurBugs
recurGol rBugs = foldr (\d -> alter (newVal d) d) rBugs activeDepths
  where
    status depth n v
      | (bugs depth `testBit` n && bugMask depth n == 1) ||
          (not (bugs depth `testBit` n) && bugMask depth n `elem` [1, 2]) =
        v `setBit` n
      | otherwise = v `clearBit` n
    bugMask depth n = left depth n + right depth n + up depth n + down depth n
    left depth n
      | n `mod` 5 == 0 = bitify (external depth) 11
      | n == 13 = sum . L.map (bitify (internal depth)) $ [4, 9, 14, 19, 24]
      | otherwise = popCount $ bit (n - 1) .&. bugs depth
    right depth n
      | n `mod` 5 == 4 = bitify (external depth) 13
      | n == 11 = sum . L.map (bitify (internal depth)) $ [0, 5, 10, 15, 20]
      | otherwise = popCount $ bit (n + 1) .&. bugs depth
    up depth n
      | n < 5 = bitify (external depth) 7
      | n == 17 = sum . L.map (bitify (internal depth)) $ [20, 21, 22, 23, 24]
      | otherwise = popCount $ bit (n - 5) .&. bugs depth
    down depth n
      | n > 19 = bitify (external depth) 17
      | n == 7 = sum . L.map (bitify (internal depth)) $ [0, 1, 2, 3, 4]
      | otherwise = popCount $ bit (n + 5) .&. bugs depth
    external depth = findWithDefault 0 (depth + 1) rBugs
    internal depth = findWithDefault 0 (depth - 1) rBugs
    bugs depth = findWithDefault 0 depth rBugs
    bitify bugLayer val = popCount $ bugLayer .&. bit val
    golLayer depth = foldr (status depth) (bugs depth) $ [0 .. 11] ++ [13 .. 24]
    newVal depth _
      | golLayer depth == 0 = Nothing
      | otherwise = Just $ golLayer depth
    activeDepths =
      toList . unions . L.map (\d -> fromList . L.map (d +) $ [-1, 0, 1]) . keys $
      rBugs

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
  | otherwise = biodiversity (St.insert bugs seen) $ gol bugs

part1 :: Bool -> String -> String
part1 _ = show . biodiversity empty . birth

part2 :: Bool -> String -> String
part2 test =
  show .
  sum . M.map popCount . (!! time) . iterate recurGol . singleton 0 . birth
  where
    time
      | test = 10
      | otherwise = 200
