module Day18
  ( part1
  , part2
  ) where

import           Data.Ix              (inRange)
import           Data.List            (inits)
import           Data.Set             (Set, empty, fromList, member, notMember)
import           Data.Text            (Text)
import           Helpers.Graph        (Pos, dirs, origin)
import           Helpers.Parsers.Text (signedInts)
import           Helpers.Search       (bfsDist, dfs)
import           Linear.V2            (V2 (..))

type Bytes = Set Pos

goal test
  | test = V2 6 6
  | otherwise = V2 70 70

range test = (origin, goal test)

shortestPath :: Bool -> Bytes -> Int
shortestPath test bytes = bfsDist origin (neighbours test bytes) (== goal test)

hasPath :: Bool -> Bytes -> Bool
hasPath test bytes =
  member (goal test) . dfs [origin] (neighbours test bytes) $ empty

neighbours :: Bool -> Bytes -> Pos -> [Pos]
neighbours test bytes pos =
  filter (\p -> inRange (range test) p && p `notMember` bytes) . map (pos +)
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
