module Day17
  ( part1
  , part2
  ) where

import           Data.Hashable (Hashable, hashWithSalt)
import           Data.Ix       (inRange)
import           Data.Text     as T (Text, empty, init, length, pack, snoc,
                                     unpack)
import           Helpers.Graph (Pos, east, north, origin, south, west)
import           Linear.V2     (V2 (..))
import           MD5           (md5Concat)

import           Debug.Trace

data Search = Search
  { getPos  :: Pos
  , getPath :: Path
  } deriving (Show, Eq, Ord)

type Path = Text

goal = V2 3 3

dirPairs = zip ['U', 'D', 'L', 'R'] [north, south, west, east]

listBFS :: [a] -> (a -> [a]) -> (a -> Bool) -> [a]
listBFS xs nexts isGoal
  | any isGoal xs = filter isGoal xs
  | otherwise = listBFS (concatMap nexts xs) nexts isGoal

listBFSLongest ::
     [Search] -> (Search -> [Search]) -> (Search -> Bool) -> Int -> Int
listBFSLongest xs nexts isGoal longest
  | null xs = longest
  | any isGoal xs =
    listBFSLongest (concatMap nexts . filter (not . isGoal) $ xs) nexts isGoal
      . T.length
      . getPath
      . head
      $ xs
  | otherwise = listBFSLongest (concatMap nexts xs) nexts isGoal longest

longestPath :: Text -> Int
longestPath passcode =
  listBFSLongest
    [Search origin T.empty]
    (neighbours passcode)
    ((== goal) . getPos)
    0

bestPath :: Text -> String
bestPath passcode =
  unpack
    . getPath
    . head
    . listBFS [Search origin T.empty] (neighbours passcode)
    $ ((== goal) . getPos)

neighbours :: Text -> Search -> [Search]
neighbours passcode (Search pos path) =
  filter (inRange (origin, goal) . getPos) . map step $ openDoors
  where
    openDoors =
      map fst
        . filter (flip elem ['b' .. 'f'] . snd)
        . zip dirPairs
        . take 4
        . md5Concat path
        $ passcode :: [(Char, Pos)]
    step (char, dir) = Search (pos + dir) (snoc path char)

part1 :: Bool -> Text -> String
part1 _ = bestPath . T.init

part2 :: Bool -> Text -> String
part2 _ = show . longestPath . T.init
