module Day16
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed as A (UArray, bounds, inRange, (!))
import           Data.List          as L (map)
import           Data.Map           as M (fromList, (!))
import           Data.Set           as St (Set, empty, map, singleton, size)
import           Helpers.Parsers    (arrayFromString)
import           Helpers.Search     (dfs)
import           Linear.V2          (V2 (..))

type Pos = (V2 Int)

type Cave = UArray Pos Char

type Beam = (Pos, Pos)

type Energized = Set Pos

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

startPos = (V2 0 0, east)

reflections =
  fromList
    [ ((north, '/'), east)
    , ((north, '\\'), west)
    , ((south, '/'), west)
    , ((south, '\\'), east)
    , ((west, '/'), south)
    , ((west, '\\'), north)
    , ((east, '/'), north)
    , ((east, '\\'), south)
    ]

edges :: Cave -> [Beam]
edges cave =
  [(V2 x 0, south) | x <- [0 .. mx]] ++
  [(V2 x my, north) | x <- [0 .. mx]] ++
  [(V2 0 y, east) | y <- [0 .. my]] ++ [(V2 mx y, west) | y <- [0 .. my]]
  where
    (_, V2 mx my) = bounds cave

neighbours :: Cave -> Beam -> [Beam]
neighbours cave beam@(pos, dir) =
  filter (\(t, _) -> inRange (bounds cave) t) newPos
  where
    newPos
      | cave A.! pos == '.' ||
          cave A.! pos == '−' && elem dir [east, west] ||
          cave A.! pos == '|' && elem dir [north, south] = [(pos + dir, dir)]
      | cave A.! pos == '-' = split [east, west]
      | cave A.! pos == '|' = split [north, south]
      | otherwise =
        [ ( pos + reflections M.! (dir, cave A.! pos)
          , reflections M.! (dir, cave A.! pos))
        ]
    split = L.map (\a -> (pos + a, a))

part1 :: Bool -> String -> String
part1 _ input =
  show . size . St.map fst . dfs [startPos] (neighbours cave) $ empty
  where
    cave = arrayFromString input

part2 :: Bool -> String -> String
part2 _ input =
  show .
  maximum .
  L.map (\x -> size . St.map fst . dfs [x] (neighbours cave) $ empty) . edges $
  cave
  where
    cave = arrayFromString input
