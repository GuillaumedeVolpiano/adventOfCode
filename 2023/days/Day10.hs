module Day10
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed as U (UArray, assocs, bounds, inRange,
                                          indices, (!))
import           Data.Graph         as G (Graph, graphFromEdges)
import           Data.Map           as M (keys, (!))
import           Data.Maybe         (fromJust)
import           Helpers.Parsers    (arrayFromString)
import           Helpers.Search     (NodeFromVertex, VertexFromKey, bfsDist,
                                     floydWarshall)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

type Diagram = UArray Pos Pipe

type Pipe = Char

type FullGraph = (Graph, NodeFromVertex Pos Pos, VertexFromKey Pos)

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

pipes =
  [ ('|', [north, south])
  , ('-', [east, west])
  , ('L', [north, east])
  , ('J', [north, west])
  , ('7', [south, west])
  , ('F', [south, east])
  , ('.', [])
  , ('S', [north, south, east, west])
  ]

findStart :: Diagram -> Pos
findStart = fst . head . filter (\(a, b) -> b == 'S') . assocs

graphFromArray :: Diagram -> FullGraph
graphFromArray diagram =
  graphFromEdges [(pos, pos, neighbours diagram pos) | pos <- indices diagram]

neighbours :: Diagram -> Pos -> [Pos]
neighbours diagram pos =
  filter (inRange $ bounds diagram) .
  map (pos +) . fromJust . lookup (diagram U.! pos) $
  pipes

neighboursNoStart :: Diagram -> Pos -> [Pos]
neighboursNoStart diagram =
  filter (\t -> diagram U.! t /= 'S') . neighbours diagram

part1 :: Bool -> String -> String
part1 _ input = show $ div fromToTo 2 + 1
  where
    diagram = arrayFromString input
    start = findStart diagram
    [from, to] =
      filter (\t -> start `elem` neighbours diagram t) . neighbours diagram $
      start
    fromToTo = bfsDist from (neighboursNoStart diagram) (== to)

--  show . maximum . map (fw M.!) . filter (\(a, _) -> a == start) . keys $ fw
--    graph@(_, nfv, vfk) = graphFromArray diagram
--    fw = floydWarshall graph
--    start = fromJust . vfk . findStart $ diagram
part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
