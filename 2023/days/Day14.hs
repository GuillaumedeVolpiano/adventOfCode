module Day14
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, bounds, inRange, (!))
import           Data.List          as L (foldl)
import           Data.Set           as St (Set, empty, filter, foldl, fromList,
                                           map, notMember, union)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

import           Debug.Trace

type Platform = UArray Pos Char

type Pos = V2 Int

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

moveOnce :: Platform -> Set Pos -> Pos -> Set Pos
moveOnce platform rocks dir = allMoved
  where
    b@(_, V2 mx my) = bounds platform
    order
      | dir == north = [0 .. my]
      | dir == south = [my,my - 1 .. 0]
      | dir == west = [0 .. mx]
      | dir == east = [mx,mx - 1 .. 0]
    coord (V2 x y)
      | dir `elem` [north, south] = y
      | otherwise = x
    allMoved = displaceByRow order empty
    displaceByRow [] moved = moved
    displaceByRow (x:xs) moved =
      displaceByRow xs .
      union moved . St.map (testPos moved) . St.filter (\p -> coord p == x) $
      rocks
    testPos seen p
      | inRange b (p + dir) &&
          (p + dir) `notMember` seen && platform ! (p + dir) /= '#' = p + dir
      | otherwise = p

move :: Platform -> Set Pos -> Pos -> Set Pos
move platform rocks dir
  | rocks == moved = rocks
  | otherwise = move platform moved dir
  where
    moved = moveOnce platform rocks dir

score :: Platform -> Set Pos -> Int
score platform = St.foldl (\a (V2 _ y) -> a + offset - y) 0
  where
    (_, V2 _ y) = bounds platform
    offset = y + 1

cycle :: Platform -> Set Pos -> Set Pos
cycle platform rocks = L.foldl (move platform) rocks [north, west, south, east]

part1 :: Bool -> String -> String
part1 _ input = show . score platform . move platform rocks $ north
  where
    platform = arrayFromString input
    (_, V2 mx my) = bounds platform
    rocks =
      fromList
        [V2 x y | x <- [0 .. mx], y <- [0 .. my], platform ! V2 x y == 'O']

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
