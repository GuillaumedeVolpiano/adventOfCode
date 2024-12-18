module Day18
  ( part1
  , part2
  ) where

import Data.Maybe (isJust)
import           Data.Ix              (inRange)
import           Data.List            (inits)
import           Data.Set             as S (Set, empty, fromList, member, notMember, toList)
import           Data.Text            (Text)
import           Helpers.Graph        (Pos, dirs, origin)
import           Helpers.Parsers.Text (signedInts)
import           Helpers.Search       (bfsDist, dfs)
import           Linear.V2            (V2 (..))
import Data.Map as M (Map, empty, notMember, insert)

type Bytes = Set Pos

goal test
  | test = V2 6 6
  | otherwise = V2 70 70

range test = (origin, goal test)

listBfsSafe :: Ord a => [a] -> Int -> Map a a -> (a -> [a]) -> (a -> Bool) -> Maybe (Int, [a], Map a a)
listBfsSafe toSee dist paths neighbours isGoal
  | null toSee = Nothing
  | any isGoal toSee = Just (dist, filter isGoal toSee, paths)
  | otherwise = listBfsSafe toSee'' (dist + 1) paths' neighbours isGoal
  where
    toSee' = [(node, filter (`M.notMember` paths) . neighbours $ node) | node <- toSee, any (`M.notMember` paths) . neighbours $ node]
    toSee'' = toList . fromList . concatMap snd $ toSee'
    paths' = foldr (\(a, b) c -> foldr (`insert` a) c b) paths toSee'

shortestPath :: Bool -> Bytes -> Int
shortestPath test bytes = (\(Just (a, _, _)) -> a) . listBfsSafe [origin] 0 M.empty (neighbours test bytes) $ (== goal test)

hasPath :: Bool -> Bytes -> Bool
hasPath test bytes =
  isJust . listBfsSafe [origin] 0 M.empty (neighbours test bytes) $ (==goal test)

neighbours :: Bool -> Bytes -> Pos -> [Pos]
neighbours test bytes pos =
  filter (\p -> inRange (range test) p && p `S.notMember` bytes) . map (pos +)
    $ dirs

part1 :: Bool -> Text -> String
part1 test =
  show
    . shortestPath test
    . fromList
    . map (\[a, b] -> V2 a b)
    . take number
    . signedInts
  where
    number
      | test = 12
      | otherwise = 1024

part2 :: Bool -> Text -> String
part2 test =
  show
    . last
    . head
    . dropWhile (hasPath test . fromList)
    . drop 1025
    . inits
    . map (\[a, b] -> V2 a b)
    . signedInts
