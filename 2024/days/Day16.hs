module Day16
  ( part1
  , part2
  ) where

import           Data.Bits                  (clearBit, complementBit, setBit,
                                             shiftR, testBit, (.&.), (.|.))
import           Data.ByteString            (ByteString, unpack)
import           Data.IntMap                (IntMap, notMember, (!))
import qualified Data.IntMap                as M (empty, singleton)
import qualified Data.IntPSQ                as Q (singleton)
import           Data.IntSet                as S (IntSet, fromList, member,
                                                  size, union, unions)
import qualified Data.IntSet                as S (empty, foldr, map)
import           Data.Maybe                 (fromJust, mapMaybe)
import           Data.Word8                 (_E, _S, _numbersign)
import qualified Helpers.Parsers.ByteString as P (lines)
import           Helpers.Search.Int         (dijkstraAllShortestPaths,
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

initialise :: ByteString -> (Reindeer, Maze, Int)
initialise input = (start, maze, goalPos)
  where
    literalMaze =
      concat
        . zipWith (\a b -> zipWith (\c d -> (256 * a + c, d)) [0 ..] b) [0 ..]
        . map unpack
        . P.lines
        $ input
    maze = fromList . fmap fst . filter ((== _numbersign) . snd) $ literalMaze
    startPos = fst . head . filter ((== _S) . snd) $ literalMaze
    goalPos = fst . head . filter ((== _E) . snd) $ literalMaze
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
        ps = paths ! p

part1 :: Bool -> ByteString -> String
part1 _ = show . dijkstra . initialise

part2 :: Bool -> ByteString -> String
part2 _ = show . allPaths . initialise
