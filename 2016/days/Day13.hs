{-# LANGUAGE TupleSections #-}

module Day13
  ( part1
  , part2
  ) where

import           Data.Bits     (popCount)
import           Data.Maybe    (mapMaybe)
import           Data.Sequence as Sq (Seq ((:<|), (:|>)), null, singleton)
import           Data.Set      as St (Set, insert, notMember, singleton, size)
import           Data.Text     as T (Text, init, unpack)
import           Data.Tuple    (swap)
import           Helpers.Graph (Pos, dirs)
import           Linear.V2     (V2 (..))

compose :: (Int, Int) -> Int
compose (x, y) = x + 50 * y

startPos = V2 1 1

isOpen :: Int -> Pos -> Bool
isOpen value (V2 x y) =
  x >= 0
    && y >= 0
    && (even . popCount $ x ^ 2 + 3 * x + 2 * x * y + y + y ^ 2 + value)

neighbours :: Int -> Pos -> [Pos]
neighbours value pos = filter (isOpen value) . map (pos +) $ dirs

bfsDist :: Int -> Seq (Pos, Int) -> Set Pos -> Pos -> Int
bfsDist value toSee seen goal
  | pos == goal = dist
  | otherwise = bfsDist value toSee' seen' goal
  where
    ((pos, dist) :<| rest) = toSee
    toConsider =
      map (, dist + 1) . filter (`notMember` seen) . neighbours value $ pos
    toSee' = foldr (flip (:|>)) rest toConsider
    seen' = foldr (insert . fst) seen toConsider

bfsAccum :: Int -> Seq (Pos, Int) -> Set Pos -> Int
bfsAccum value toSee seen
  | Sq.null toSee = size seen
  | otherwise = bfsAccum value toSee' seen'
  where
    ((pos, dist) :<| rest) = toSee
    toConsider
      | dist == 50 = []
      | otherwise =
        map (, dist + 1) . filter (`notMember` seen) . neighbours value $ pos
    toSee' = foldr (flip (:|>)) rest toConsider
    seen' = foldr (insert . fst) seen toConsider

part1 :: Bool -> Text -> String
part1 test input =
  show . bfsDist value (Sq.singleton (startPos, 0)) (St.singleton startPos)
    $ goal
  where
    goal
      | test = V2 7 4
      | otherwise = V2 31 39
    value = read . unpack . T.init $ input

part2 :: Bool -> Text -> String
part2 _ input =
  show
    . bfsAccum (read . unpack . T.init $ input) (Sq.singleton (startPos, 0))
    . St.singleton
    $ startPos
