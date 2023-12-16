module Day16
  ( part1
  , part2
  ) where

import           Control.Parallel.Strategies (parMap, rpar)
import           Data.Array.IArray           as I (Array, array, (!))
import           Data.Array.Unboxed          as U (Ix, UArray, bounds, inRange,
                                                   range, (!))
import           Data.List                   as L (map)
import           Data.Map                    as M (fromList, (!))
import           Data.Set                    as St (Set, empty, map, singleton,
                                                    size)
import           Helpers.Parsers             (arrayFromString)
import           Helpers.Search              (dfs)
import           Linear.V2                   (V2 (..))

type Pos = (V2 Int)

type Cave = UArray Pos Char

data Beam =
  Beam Pos Pos
  deriving (Show, Eq, Ord, Ix)

type Energized = Set Pos

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

startPos = Beam (V2 0 0) east

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
  [Beam (V2 x 0) south | x <- [0 .. mx]] ++
  [Beam (V2 x my) north | x <- [0 .. mx]] ++
  [Beam (V2 0 y) east | y <- [0 .. my]] ++
  [Beam (V2 mx y) west | y <- [0 .. my]]
  where
    (_, V2 mx my) = bounds cave

neighbours :: Cave -> Beam -> [Beam]
neighbours cave beam@(Beam pos dir) =
  filter (\(Beam t _) -> inRange (bounds cave) t) newPos
  where
    newPos
      | cave U.! pos == '.' ||
          cave U.! pos == '−' && elem dir [east, west] ||
          cave U.! pos == '|' && elem dir [north, south] =
        [Beam (pos + dir) dir]
      | cave U.! pos == '-' = split [east, west]
      | cave U.! pos == '|' = split [north, south]
      | otherwise =
        [ Beam
            (pos + reflections M.! (dir, cave U.! pos))
            (reflections M.! (dir, cave U.! pos))
        ]
    split = L.map (\a -> Beam (pos + a) a)

toPos :: Beam -> Pos
toPos (Beam p _) = p

part1 :: Bool -> String -> String
part1 _ input =
  show . size . St.map toPos . dfs [startPos] (neighbours cave) $ empty
  where
    cave = arrayFromString input

part2 :: Bool -> String -> String
part2 _ input =
  show .
  maximum .
  parMap rpar (\x -> size . St.map toPos . dfs [x] (allBeams I.!) $ empty) .
  edges $
  cave
  where
    cave = arrayFromString input
    (start, end@(V2 mx my)) = bounds cave
    allBeams =
      array
        (Beam start (V2 (-1) (-1)), Beam end (V2 1 1))
        [ (Beam (V2 x y) dir, neighbours cave (Beam (V2 x y) dir))
        | x <- [0 .. mx]
        , y <- [0 .. my]
        , dir <- [north, south, east, west]
        ] :: Array Beam [Beam]
