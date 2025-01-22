module Day14
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed         (UArray, bounds, inRange, (!))
import           Data.ByteString            (ByteString)
import           Data.List                  as L (foldl', map)
import           Data.Maybe                 (fromJust)
import           Data.Sequence              as Sq (Seq ((:<|)), drop, iterateN,
                                                   length, take, takeWhileL,
                                                   (!?))
import           Data.Set                   as St (Set, difference, empty,
                                                   filter, foldl, fromList,
                                                   intersection, map, notMember,
                                                   null, union)
import           Data.Word                  (Word8)
import           Data.Word8                 (_O, _numbersign)
import           Helpers.Parsers.ByteString (arrayFromByteString)
import           Helpers.Search             (findPattern)
import           Linear.V2                  (V2 (..))

type Platform = UArray Pos Word8

type Pos = V2 Int

type Rocks = Set Pos

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

numCycles = 1000000000

move :: Platform -> Rocks -> Pos -> Rocks
move platform toMove dir = allMoved
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
    allMoved = foldl' displaceByRow empty order
    displaceByRow moved x = moved `union` (St.map (fullMove moved) . atX $ x)
    atX x = St.filter (\p -> coord p == x) toMove
    canMove seen p =
      inRange b (p + dir)
        && (p + dir) `notMember` seen
        && platform ! (p + dir) /= _numbersign
    fullMove moved p
      | canMove moved p = fullMove moved (p + dir)
      | otherwise = p

score :: Platform -> Rocks -> Int
score platform = St.foldl (\a (V2 _ y) -> a + offset - y) 0
  where
    (_, V2 _ y) = bounds platform
    offset = y + 1

cycleRocks :: Platform -> Rocks -> Rocks
cycleRocks platform rocks =
  L.foldl (move platform) rocks [north, west, south, east]

part1 :: Bool -> ByteString -> String
part1 _ input = show . score platform . move platform rocks $ north
  where
    platform = arrayFromByteString input
    (_, V2 mx my) = bounds platform
    rocks =
      St.fromList
        [V2 x y | x <- [0 .. mx], y <- [0 .. my], platform ! V2 x y == _O]

part2 :: Bool -> ByteString -> String
part2 _ input = show pos
  where
    platform = arrayFromByteString input
    (_, V2 mx my) = bounds platform
    rocks =
      St.fromList
        [V2 x y | x <- [0 .. mx], y <- [0 .. my], platform ! V2 x y == _O]
    firstCycles =
      fmap (score platform) . iterateN 250 (cycleRocks platform) $ rocks
    pat = findPattern 100 1 (==) firstCycles
    remainder = mod (numCycles - 100) pat
    pos = fromJust $ firstCycles !? (100 + remainder)
