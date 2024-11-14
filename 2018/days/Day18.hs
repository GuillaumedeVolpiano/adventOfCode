module Day18
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (assocs)
import           Data.List          as L (filter, map)
import           Data.Map           as M (Map, elems, empty, filter,
                                          foldrWithKey, fromList, insert,
                                          lookup, map, size)
import           Data.Sequence      as S (fromList)

import           Helpers.Graph      (Pos, east, north, south, west)
import           Helpers.Parsers    (arrayFromString)
import           Helpers.Search     (findPattern)

type Scan = Map Pos State

data State
  = Open
  | Wooded
  | Lumberyard
  deriving (Ord, Eq)

instance Show State where
  show Open       = "."
  show Wooded     = "|"
  show Lumberyard = "#"

adjacent =
  [ north
  , south
  , east
  , west
  , north + east
  , north + west
  , south + east
  , south + west
  ]

longTime = 1000000000

minute :: Scan -> Scan
minute scan = foldrWithKey evolve scan scan
  where
    evolve p Open ns
      | (>= 3) . length . L.filter (== Just Wooded) $ neighbours =
        insert p Wooded ns
      | otherwise = ns
      where
        neighbours = L.map (\a -> M.lookup (p + a) scan) adjacent
    evolve p Wooded ns
      | (>= 3) . length . L.filter (== Just Lumberyard) $ neighbours =
        insert p Lumberyard ns
      | otherwise = ns
      where
        neighbours = L.map (\a -> M.lookup (p + a) scan) adjacent
    evolve p Lumberyard ns
      | Just Lumberyard `elem` neighbours && Just Wooded `elem` neighbours = ns
      | otherwise = insert p Open ns
      where
        neighbours = L.map (\a -> M.lookup (p + a) scan) adjacent

parseInput :: String -> Scan
parseInput = M.map readState . M.fromList . assocs . arrayFromString
  where
    readState '.' = Open
    readState '|' = Wooded
    readState '#' = Lumberyard

score1 :: Scan -> Int
score1 s = wooded * lumberyards
  where
    wooded = size . M.filter (== Wooded) $ s
    lumberyards = size . M.filter (== Lumberyard) $ s

score2 :: [Scan] -> Int
score2 s = wooded * lumberyards
  where
    wooded = size . M.filter (== Wooded) $ pat
    lumberyards = size . M.filter (== Lumberyard) $ pat
    period = findPattern 1000 50 (==) . S.fromList $ s
    pat = s !! (1000 + mod (longTime - 1000) period)

part1 :: Bool -> String -> String
part1 _ = show . score1 . last . take 11 . iterate minute . parseInput

part2 :: Bool -> String -> String
part2 _ = show . score2 . take 10000 . iterate minute . parseInput
