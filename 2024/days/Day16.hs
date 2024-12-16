module Day16
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed   as A (UArray, array, assocs, (!))
import           Data.Hashable        (Hashable, hashWithSalt)
import           Data.HashPSQ         as Q (HashPSQ, insert, minView, null,
                                            singleton)
import           Data.Map             as M (Map, alter, empty, insert,
                                            notMember, singleton, (!))
import           Data.Maybe           (fromJust, mapMaybe)
import           Data.Set             as S (Set, empty, insert, map, singleton,
                                            size, union, unions)
import           Data.Text            (Text)
import           Debug.Trace
import           Helpers.Graph        (Pos, dirs, east, left, right)
import           Helpers.Parsers.Text (arrayFromText)
import           Helpers.Search       (dijkstraUncertainGoalDist)

type Maze = UArray Pos Char

type Paths = Map Reindeer (Set Reindeer)

type Dists = Map Reindeer Int

data Reindeer = Reindeer
  { pos :: Pos
  , dir :: Dir
  } deriving (Show, Eq, Ord)

type Dir = Pos

instance Hashable Reindeer where
  hashWithSalt s (Reindeer p d) = hashWithSalt s (p, d)

dijkstra :: (Reindeer, Maze, Pos) -> Int
dijkstra (start, maze, goalPos) =
  dijkstraUncertainGoalDist start 0 (neighbours maze) ((== goalPos) . pos)

initialise :: Text -> (Reindeer, Maze, Pos)
initialise input = (start, maze, goalPos)
  where
    maze = arrayFromText input
    startPos = fst . head . filter ((== 'S') . snd) . assocs $ maze
    goalPos = fst . head . filter ((== 'E') . snd) . assocs $ maze
    start = Reindeer startPos east

neighbours :: Maze -> Reindeer -> [(Reindeer, Int)]
neighbours maze reindeer
  | maze A.! (pos reindeer + dir reindeer) == '#' = turn reindeer
  | otherwise =
    (reindeer {pos = pos reindeer + dir reindeer}, 1) : turn reindeer
  where
    turn reindeer =
      [ (reindeer {dir = left . dir $ reindeer}, 1000)
      , (reindeer {dir = right . dir $ reindeer}, 1000)
      ]

-- we need the +1 because we're not counting the goal pos
allPaths :: (Reindeer, Maze, Pos) -> Int
allPaths init@(start, maze, goalPos) =
  (1 +) . size . S.map pos . unions . fmap (reconstruct . Reindeer goalPos)
    $ dirs
  where
    score = dijkstra init
    paths =
      specialDijkstra
        (Q.singleton start 0 start)
        score
        (M.singleton start 0)
        M.empty
        (neighbours maze)
    reconstruct p
      | p `notMember` paths = S.empty
      | otherwise = ps `union` foldr (union . reconstruct) S.empty ps
      where
        ps = paths M.! p

specialDijkstra ::
     HashPSQ Reindeer Int Reindeer
  -> Int
  -> Dists
  -> Paths
  -> (Reindeer -> [(Reindeer, Int)])
  -> Paths
specialDijkstra queue score dists paths nexts
  | Q.null queue = paths
  | otherwise = specialDijkstra queue' score dists' paths' nexts
  where
    (reindeer, estDist, _, rest) = fromJust . minView $ queue
    toConsider = mapMaybe consider . nexts $ reindeer
    queue' = foldr (\(b, c) -> Q.insert b c b) rest toConsider
    dists' = foldr (uncurry M.insert) dists toConsider
    paths' = foldr (M.alter (update reindeer) . fst) paths toConsider
    update p Nothing   = Just . S.singleton $ p
    update p (Just ps) = Just . S.insert p $ ps
    consider (aReindeer, anEdge)
      | estDist' <= score
          && (aReindeer `notMember` dists || estDist' <= dists M.! aReindeer) =
        Just (aReindeer, estDist')
      | otherwise = Nothing
      where
        estDist' = estDist + anEdge

part1 :: Bool -> Text -> String
part1 _ = show . dijkstra . initialise

part2 :: Bool -> Text -> String
part2 _ = show . allPaths . initialise
