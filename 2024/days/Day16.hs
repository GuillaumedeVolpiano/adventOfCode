module Day16
  ( part1
  , part2
  ) where

import           Data.Bits          (clearBit, complementBit, setBit, shiftR,
                                     testBit, (.&.), (.|.))
import           Data.Hashable      (Hashable, hashWithSalt)
import           Data.IntMap        as M (IntMap, empty, notMember, singleton,
                                          (!))
import           Data.IntPSQ        as Q (singleton)
import           Data.IntSet        as S (IntSet, empty, foldr, fromList, map,
                                          member, size, union, unions)
import           Data.Maybe         (fromJust, mapMaybe)
import           Data.Text          (Text, unpack)
import           Helpers.Search.Int (dijkstraAllShortestPaths,
                                     dijkstraUncertainGoalVal)

type Maze = IntSet

type Paths = IntMap IntSet

type Dists = IntMap Int

-- A reindeer is an 18 bits digit, with the leftmost two bits encoding the
-- direction, the next 8 bits the y axis and the last 8 bits the x axis
type Reindeer = Int

type Dir = Int

type Dist = Int

left :: Reindeer -> Reindeer
left reindeer
  | testBit reindeer 16 = reindeer `clearBit` 16
  | otherwise = flip complementBit 17 . flip setBit 16 $ reindeer
  -- 2^17 - 1

right :: Reindeer -> Reindeer
right reindeer
  | testBit reindeer 16 = flip complementBit 17 . flip clearBit 16 $ reindeer
  | otherwise = reindeer `setBit` 16

move :: Reindeer -> Reindeer
move reindeer = reindeer + delta
  where
    delta =
      case shiftR reindeer 16 of
        0 -> (-256)
        1 -> 1
        2 -> 256
        3 -> (-1)

dijkstra :: (Reindeer, Maze, Int) -> Int
dijkstra (start, maze, goalPos) =
  dijkstraUncertainGoalVal
    start
    0
    (neighbours maze)
    ((== goalPos) . (.&. 65535)) -- 2^16 - 1

initialise :: Text -> (Reindeer, Maze, Int)
initialise input = (start, maze, goalPos)
  where
    literalMaze =
      concat
        . zipWith (\a b -> zipWith (\c d -> (256 * a + c, d)) [0 ..] b) [0 ..]
        . lines
        . unpack
        $ input
    maze = fromList . fmap fst . filter ((== '#') . snd) $ literalMaze
    startPos = fst . head . filter ((== 'S') . snd) $ literalMaze
    goalPos = fst . head . filter ((== 'E') . snd) $ literalMaze
    start = right startPos

neighbours :: Maze -> Reindeer -> [(Reindeer, Int)]
neighbours maze reindeer
  | (move reindeer .&. 65535) `S.member` maze = turn reindeer -- 2^16 - 1
  | otherwise = (move reindeer, 1) : turn reindeer
  where
    turn reindeer = [(left reindeer, 1000), (right reindeer, 1000)]

allPaths :: (Reindeer, Maze, Int) -> Int
-- we need the +1 because we're not counting the goal pos
allPaths (reindeer, maze, goalPos) =
  (1 +) . size . unions . fmap (reconstruct . (+ goalPos) . (* 2 ^ 16))
    $ [0 .. 3]
  where
    paths =
      dijkstraAllShortestPaths
        (Q.singleton reindeer 0 ())
        (M.singleton reindeer 0)
        M.empty
        (neighbours maze)
        ((== goalPos) . (.&. 65535)) -- 2^16 - 1
    reconstruct p
      | p `notMember` paths = S.empty
      | otherwise =
        S.map (.&. 65535) ps `union` S.foldr (union . reconstruct) S.empty ps
      where
        ps = paths M.! p

part1 :: Bool -> Text -> String
part1 _ = show . dijkstra . initialise

part2 :: Bool -> Text -> String
part2 _ = show . allPaths . initialise
