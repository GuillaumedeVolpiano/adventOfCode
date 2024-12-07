module Day6
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, assocs, (!?))
import           Data.Hashable      (Hashable, hashWithSalt)
import           Data.HashSet       as S (HashSet, empty, filter, insert,
                                          member, size)
import           Data.List          as L (filter, unfoldr)
import           Data.Maybe         (isNothing)
import           Helpers.Graph      (Pos, left, north, right)
import           Helpers.Parsers    (arrayFromString)

data Guard =
  Guard Pos Dir
  deriving (Show, Eq, Ord)

type Dir = Pos

type Map = UArray Pos Char

type Seen = HashSet Pos

type FullSeen = HashSet Guard

type Obstacle = Pos

instance Hashable Guard where
  hashWithSalt s (Guard p d) = hashWithSalt s (p, d)

getGuard :: Map -> Guard
getGuard map = Guard pos north
  where
    pos = fst . head . L.filter ((== '^') . snd) . assocs $ map

followGuard :: Map -> (Seen, Guard) -> Maybe (Seen, (Seen, Guard))
followGuard map (seen, guard@(Guard pos dir))
  | isNothing . (!?) map $ pos = Nothing
  | map !? pos == Just '#' = Just (seen, (seen, backtrack guard))
  | otherwise = Just (seen', (seen', guard'))
  where
    pos' = pos + dir
    seen' = insert pos seen
    guard' = Guard pos' dir

backtrack :: Guard -> Guard
backtrack (Guard pos dir) = Guard pos' dir'
  where
    dir' = right dir
    pos' = pos - dir + dir'

track :: Map -> HashSet Pos
track map = last . unfoldr (followGuard map) $ (empty, guard)
  where
    guard = getGuard map

isLoop :: Map -> Guard -> FullSeen -> Obstacle -> Bool
isLoop map guard@(Guard pos dir) seen obstacle
  | guard `member` seen = True
  | isNothing . (!?) map $ pos = False
  | pos == obstacle || map !? pos == Just '#' =
    isLoop map (backtrack guard) seen' obstacle
  | otherwise = isLoop map guard' seen obstacle
  where
    pos' = pos + dir
    guard' = Guard pos' dir
    seen' = insert guard seen

findLoops :: Map -> Int
findLoops map = size . S.filter (isLoop map guard empty) . track $ map
  where
    guard = getGuard map

part1 :: Bool -> Text -> String
part1 _ = show . size . track . arrayFromString

part2 :: Bool -> Text -> String
part2 _ = show . findLoops . arrayFromString
