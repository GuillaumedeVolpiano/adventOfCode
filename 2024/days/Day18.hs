module Day18
  ( part1
  , part2
  ) where

import           Data.Ix              (inRange)
import           Data.List            (inits)
import           Data.Maybe           (fromJust, isJust)
import           Data.Set             (Set, fromList, notMember)
import           Data.Text            (Text)
import           Helpers.Graph        (Pos, dirs, origin)
import           Helpers.Parsers.Text (signedInts)
import           Helpers.Search       (bfsSafeDist)
import           Linear.V2            (V2 (..))

type Bytes = Set Pos

goal test
  | test = V2 6 6
  | otherwise = V2 70 70

range test = (origin, goal test)

shortestPath :: Bool -> Bytes -> Maybe Int
shortestPath test bytes =
  bfsSafeDist origin (neighbours test bytes) (== goal test)

neighbours :: Bool -> Bytes -> Pos -> [Pos]
neighbours test bytes pos =
  filter (\p -> inRange (range test) p && p `notMember` bytes) . map (pos +)
    $ dirs

part1 :: Bool -> Text -> String
part1 test =
  show
    . fromJust
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
    . dropWhile (isJust . shortestPath test . fromList)
    . drop 1025
    . inits
    . map (\[a, b] -> V2 a b)
    . signedInts
