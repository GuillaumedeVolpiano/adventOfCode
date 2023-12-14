module Day14
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, bounds, inRange, (!))
import           Data.List          as L (foldl, map)
import           Data.Maybe         (fromJust)
import           Data.Sequence      as Sq (Seq ((:<|)), drop, iterateN, length,
                                           take, takeWhileL, (!?))
import           Data.Set           as St (Set, difference, empty, filter,
                                           foldl, fromList, intersection, map,
                                           notMember, null, union)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

import           Debug.Trace

type Platform = UArray Pos Char

type Pos = V2 Int

type Rocks = Set Pos

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

numCycles = 1000000000

moveOnce :: Platform -> (Rocks, Rocks) -> Pos -> (Rocks, Rocks)
moveOnce platform (toMove, unMoved) dir = allMoved
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
    allMoved = displaceByRow order (empty, unMoved)
    displaceByRow [] (a, b) = (a, b)
    displaceByRow (x:xs) (moved, notMoving) =
      displaceByRow
        xs
        ( moved `union` (haveMoved moved notMoving . atX $ x)
        , notMoving `union` (cantMove moved notMoving . atX $ x))
    atX x = St.filter (\p -> coord p == x) toMove
    haveMoved moved staid = St.map (+ dir) . St.filter (canMove moved staid)
    cantMove moved staid = St.filter (not . canMove moved staid)
    canMove seen others p =
      inRange b (p + dir) &&
      (p + dir) `notMember` seen &&
      (p + dir) `notMember` others && platform ! (p + dir) /= '#'

move :: Platform -> (Rocks, Rocks) -> Pos -> Rocks
move platform rocks dir
  | St.null moved = newUnmoved
  | otherwise = move platform (moved, newUnmoved) dir
  where
    (moved, newUnmoved) = moveOnce platform rocks dir

score :: Platform -> Rocks -> Int
score platform = St.foldl (\a (V2 _ y) -> a + offset - y) 0
  where
    (_, V2 _ y) = bounds platform
    offset = y + 1

cycleRocks :: Platform -> Rocks -> Rocks
cycleRocks platform rocks =
  L.foldl (\a b -> move platform (a, empty) b) rocks [north, west, south, east]

findPattern :: Int -> Int -> Seq Int -> Int
findPattern startPoint minSize rockCycle
  | (startPoint + minSize) >= div (Sq.length rockCycle) 2 =
    error "Could not find a pattern"
  | testPattern = potPatternLength
  | otherwise = findPattern startPoint (potPatternLength + 1) rockCycle
  where
    pruned = Sq.drop startPoint rockCycle
    (testRocks :<| rest) = pruned
    potPatternLength =
      minSize + Sq.length (takeWhileL (/= testRocks) $ Sq.drop minSize pruned)
    testPattern = fromJust (pruned !? (2 * potPatternLength)) == testRocks

part1 :: Bool -> String -> String
part1 _ input = show . score platform . move platform (rocks, empty) $ north
  where
    platform = arrayFromString input
    (_, V2 mx my) = bounds platform
    rocks =
      St.fromList
        [V2 x y | x <- [0 .. mx], y <- [0 .. my], platform ! V2 x y == 'O']

part2 :: Bool -> String -> String
part2 _ input = show pos
  where
    platform = arrayFromString input
    (_, V2 mx my) = bounds platform
    rocks =
      St.fromList
        [V2 x y | x <- [0 .. mx], y <- [0 .. my], platform ! V2 x y == 'O']
    firstCycles =
      fmap (score platform) . iterateN 250 (cycleRocks platform) $ rocks
    pat = findPattern 100 1 firstCycles
    remainder = mod (numCycles - 100) pat
    pos = fromJust $ firstCycles !? (100 + remainder)
