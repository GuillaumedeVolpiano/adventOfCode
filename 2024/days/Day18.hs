module Day18
  ( part1
  , part2
  ) where

import           Data.Ix              (inRange)
import           Data.List            (inits)
import           Data.List.Split      (splitOn)
import           Data.Maybe           (fromJust)
import           Data.Set             (Set, empty, fromList, member, notMember)
import           Data.Text            (Text, unpack)
import           Helpers.Graph        (Pos, dirs, origin)
import           Helpers.Parsers.Text (signedInts)
import           Helpers.Search       (bfsSafeDist, dfs)
import           Linear.V2            (V2 (..))

type Bytes = Set Pos

goal test
  | test = V2 6 6
  | otherwise = V2 70 70

range test = (origin, goal test)

shortestPath :: Bool -> Bytes -> Int
shortestPath test bytes =
  fromJust . bfsSafeDist origin (neighbours test bytes) $ (== goal test)

hasPath :: Bool -> Bytes -> Bool
hasPath test bytes =
  member (goal test) . dfs [origin] (neighbours test bytes) $ empty

neighbours :: Bool -> Bytes -> Pos -> [Pos]
neighbours test bytes pos =
  filter (\p -> inRange (range test) p && p `notMember` bytes) . map (pos +)
    $ dirs

binary :: Bool -> Int -> Int -> [Pos] -> Pos
binary test low high bytes
  | low > length bytes = error "notFound"
  | hasPath test highBytes = binary test high (2 * high) bytes
  | low == high && not (hasPath test lowBytes) = bytes !! (low - 1)
  | low == high = error "not found"
  | high == low + 1 && hasPath test lowBytes = bytes !! (high - 1)
  | high == low + 1 = bytes !! (low - 1)
  | not (hasPath test lowBytes) = binary test low (div low 2) bytes
  | hasPath test midBytes = binary test mid high bytes
  | otherwise = binary test low mid bytes
  where
    highBytes = fromList . take high $ bytes
    lowBytes = fromList . take low $ bytes
    midBytes = fromList . take mid $ bytes
    mid = div (high + low) 2

part1 :: Bool -> Text -> String
part1 test =
  show
    . shortestPath test
    . fromList
    . take number
    . map ((\[a, b] -> V2 (read a) (read b)) . splitOn ",")
    . lines
    . unpack
  where
    number
      | test = 12
      | otherwise = 1024

part2 :: Bool -> Text -> String
part2 test =
  show
    . binary test 1024 2048
    . map ((\[a, b] -> V2 (read a) (read b)) . splitOn ",")
    . lines
    . unpack
