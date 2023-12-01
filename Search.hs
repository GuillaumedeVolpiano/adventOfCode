module Search
  ( NodeFromVertex
  , VertexFromKey
  , floydWarshall
  ) where

import           Data.Graph  (Edge, Graph, Vertex, edges, vertices)
import           Data.Map    (Map, empty, insert, member, notMember, (!))

import           Debug.Trace

type NodeFromVertex a = Vertex -> (Int, a, [a])

type VertexFromKey a = a -> Maybe Vertex

floydWarshall ::
     Ord a => (Graph, NodeFromVertex a, VertexFromKey a) -> Map Edge Int
floydWarshall (graph, nodeFromVertex, _) = shortPaths valVertices distEdges
  where
    valVertices = vertices graph
    distVertices = foldl (\a b -> insert (b, b) 0 a) empty valVertices
    distEdges = foldl (\a b -> insert b 1 a) distVertices $ edges graph
    shortPaths [] dists     = dists
    shortPaths (x:xs) dists = shortPaths xs $ findPaths x valVertices dists
    findPaths _ [] dists     = dists
    findPaths x (y:ys) dists = findPaths x ys $ path x y valVertices dists
    path _ _ [] dists = dists
    path x y (z:zs) dists
      | not (testMemberships x y z dists) = path x y zs dists
      | notMember (y, z) dists =
        path x y zs $ insert (y, z) (dists ! (y, x) + dists ! (x, z)) dists
      | dists ! (y, z) > dists ! (y, x) + dists ! (x, z) =
        path x y zs $ insert (y, z) (dists ! (y, x) + dists ! (x, z)) dists
      | otherwise = path x y zs dists
    testMemberships x y z dists = member (y, x) dists && member (x, z) dists
