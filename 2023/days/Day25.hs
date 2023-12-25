module Day25
  ( part1
  , part2
  ) where

import           Data.Graph.Inductive.Graph        (Edge, LNode, Node, Path,
                                                    delEdge, delNode, edges,
                                                    insEdge, labEdges,
                                                    neighbors, nodes)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.Graph.Inductive.Query.BFS    (bft)
import           Data.Graph.Inductive.Query.DFS    (scc)
import           Data.List                         (maximumBy)
import           Data.Map                          (Map, fromList, (!))
import           Data.Text.Lazy                    (unpack)
import           Helpers.Graph                     (dicToGraph, graphToViz)
import           Helpers.Parsers                   (complexParser)

import           Debug.Trace

type Component = String

type Diagram = Gr String String

findFurthest :: Node -> Diagram -> Path
findFurthest node = maximumBy (\a b -> compare (length a) (length b)) . bft node

digraph :: Diagram -> Diagram
digraph diagram =
  foldl (\a (b, c, d) -> insEdge (c, b, d) a) diagram . labEdges $ diagram

cutDiagram :: Diagram -> Diagram
cutDiagram diagram = thirdCut
  where
    furPath = findFurthest 0 . digraph $ diagram
    firstCut = cutPath furPath . digraph $ diagram
    secondCut = cutPath (findFurthest 1 firstCut) firstCut
    thirdCut = cutPath (findFurthest 2 secondCut) secondCut

cutPath :: Path -> Diagram -> Diagram
cutPath [a] d      = d
cutPath (a:b:as) d = cutPath (b : as) . delEdge (a, b) . delEdge (b, a) $ d

part1 :: Bool -> String -> String
part1 _ =
  show .
  product .
  map length .
  scc .
  cutDiagram .
  dicToGraph .
  fromList .
  map ((\(a:b) -> (init a, map (\c -> (c, init a ++ " " ++ c)) b)) . words) .
  lines

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
