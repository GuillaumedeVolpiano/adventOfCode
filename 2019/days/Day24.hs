module Day24
  ( part1
  , part2
  ) where

import           Control.Lens.Getter ((^.))
import           Data.Array.Unboxed  (UArray, bounds, inRange, indices, (!))
import           Data.List           as L (filter, map)
import           Data.Set            as St (Set, empty, filter, fromList,
                                            insert, intersection, map, member,
                                            singleton, size, union, unions)
import           Helpers.Graph       (Pos, dirs)
import           Helpers.Parsers     (boolArrayFromString)
import           Linear.V2           (V2 (..))
import           Linear.V3           (V3 (..), _xy, _z)

type RecurPos = V3 Int

type RecurBugs = Set RecurPos

type Bugs = Set Pos

range = (V2 0 0, V2 4 4)

recurBirth :: UArray Pos Bool -> RecurBugs
recurBirth = St.map (\(V2 x y) -> V3 x y 0) . birth

birth :: UArray Pos Bool -> Bugs
birth bugMap = St.filter (bugMap !) . fromList . indices $ bugMap

recurGol :: RecurBugs -> RecurBugs
recurGol bugs = survive `union` rebirth
  where
    survive = St.filter survivor bugs
    survivor = (1 ==) . countNeighbours . neighbours
    candidates =
      foldr union empty . St.map (St.filter (not . flip elem bugs) . neighbours) $
      bugs
    rebirth = St.filter reborn candidates
    reborn = flip elem [1, 2] . countNeighbours . neighbours
    left (V3 x y z)
      | x == 0 = singleton $ V3 1 2 (z + 1)
      | x == 3 && y == 2 = fromList [V3 4 a (z - 1) | a <- [0 .. 4]]
      | otherwise = singleton $ V3 (x - 1) y z
    right (V3 x y z)
      | x == 4 = singleton $ V3 3 2 (z + 1)
      | x == 1 && y == 2 = fromList [V3 0 a (z - 1) | a <- [0 .. 4]]
      | otherwise = singleton $ V3 (x + 1) y z
    up (V3 x y z)
      | y == 0 = singleton $ V3 2 1 (z + 1)
      | y == 3 && x == 2 = fromList [V3 a 4 (z - 1) | a <- [0 .. 4]]
      | otherwise = singleton $ V3 x (y - 1) z
    down (V3 x y z)
      | y == 4 = singleton $ V3 2 3 (z + 1)
      | y == 1 && x == 2 = fromList [V3 a 0 (z - 1) | a <- [0 .. 4]]
      | otherwise = singleton $ V3 x (y + 1) z
    neighbours = left <> right <> up <> down
    countNeighbours = size . intersection bugs

gol :: Bugs -> Bugs
gol bugs = survive `union` rebirth
  where
    survive = St.filter survivor bugs
    survivor p = (1 ==) . length . L.filter (`member` bugs) . L.map (p +) $ dirs
    candidates =
      St.filter (not . flip elem bugs) .
      unions . L.map (\p -> St.map (p +) bugs) $
      dirs
    rebirth = St.filter reborn candidates
    reborn p =
      inRange range p &&
      (flip elem [1, 2] . length . L.filter (`member` bugs) . L.map (p +) $ dirs)

biodiversity :: Set Bugs -> Bugs -> Int
biodiversity layout bugs
  | bugs `member` layout = sum . St.map biodiv $ bugs
  | otherwise = biodiversity (insert bugs layout) $ gol bugs
  where
    biodiv (V2 x y) = 2 ^ (5 * y + x)

part1 :: Bool -> String -> String
part1 _ = show . biodiversity empty . birth . boolArrayFromString '#'

part2 :: Bool -> String -> String
part2 test =
  show .
  size . (!! time) . iterate recurGol . recurBirth . boolArrayFromString '#'
  where
    time
      | test = 10
      | otherwise = 200
