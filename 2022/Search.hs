module Search
  ( NodeFromVertex
  , VertexFromKey
  , astar
  , astarVal
  , dfs
  , dfsBest
  , floydWarshall
  ) where

import           Data.Graph    (Edge, Graph, Vertex, edges, vertices)
import           Data.Hashable (Hashable)
import           Data.HashPSQ  as Q (HashPSQ, insert, lookup, minView, null,
                                     singleton)
import           Data.Map      as M (Map, empty, insert, lookup, member,
                                     notMember, singleton, (!))
import           Data.Maybe    (fromJust, isNothing)
import           Data.Set      as St (Set, empty, insert, member)

import           Debug.Trace

type NodeFromVertex a = Vertex -> (Int, a, [a])

type VertexFromKey a = a -> Maybe Vertex

-- Find all shortest paths in a graph
floydWarshall ::
     Ord a => (Graph, NodeFromVertex a, VertexFromKey a) -> Map Edge Int
floydWarshall (graph, nodeFromVertex, _) = shortPaths valVertices distEdges
  where
    valVertices = vertices graph
    distVertices = foldl (\a b -> M.insert (b, b) 0 a) M.empty valVertices
    distEdges = foldl (\a b -> M.insert b 1 a) distVertices $ edges graph
    shortPaths [] dists     = dists
    shortPaths (x:xs) dists = shortPaths xs $ findPaths x valVertices dists
    findPaths _ [] dists     = dists
    findPaths x (y:ys) dists = findPaths x ys $ path x y valVertices dists
    path _ _ [] dists = dists
    path x y (z:zs) dists
      | not (testMemberships x y z dists) = path x y zs dists
      | notMember (y, z) dists =
        path x y zs $ M.insert (y, z) (dists ! (y, x) + dists ! (x, z)) dists
      | dists ! (y, z) > dists ! (y, x) + dists ! (x, z) =
        path x y zs $ M.insert (y, z) (dists ! (y, x) + dists ! (x, z)) dists
      | otherwise = path x y zs dists
    testMemberships x y z dists = M.member (y, x) dists && M.member (x, z) dists

-- Depth first search
dfs :: Ord n => [n] -> (n -> [n]) -> Set n -> Set n
dfs [] _ seen = seen
dfs (node:ns) neighbours seen
  | St.member node seen = dfs ns neighbours seen
  | otherwise = dfs (neighbours node ++ ns) neighbours $ St.insert node seen

dfsBest :: Ord n => [n] -> k -> (n -> k -> [n]) -> (n -> k -> k) -> Set n -> k
dfsBest [] curBest _ _ _ = curBest
dfsBest (node:ns) curBest neighbours checkBest seen
  | St.member node seen = dfsBest ns curBest neighbours checkBest seen
  | otherwise =
    dfsBest
      (neighbours node curBest ++ ns)
      (checkBest node curBest)
      neighbours
      checkBest $
    St.insert node seen

--A* search
astarVal ::
     (Hashable k, Ord k, Num p, Ord p)
  => k
  -> v
  -> (k -> Bool)
  -> (k -> [(k, v)])
  -> (k -> p)
  -> (v -> v -> p)
  -> p
astarVal startKey startValue isGoal neighbours heuristic dist =
  fst $ astar startKey startValue isGoal neighbours heuristic dist

-- queue: a priority queue, originally the start key priority value set
-- isGoal: returns True if the goal has been reached
-- neighbours: a function that returns a list of key  values of the nodes
--            accessible from the current node.
-- heuristic: the heuristic function
-- dist : a function returning the real distance between two values
astar ::
     (Hashable k, Ord k, Num p, Ord p)
  => k
  -> v
  -> (k -> Bool)
  -> (k -> [(k, v)])
  -> (k -> p)
  -> (v -> v -> p)
  -> (p, [v])
astar startKey startValue =
  astarMech (Q.singleton startKey 0 startValue) M.empty (M.singleton startKey 0)

astarMech ::
     (Hashable k, Ord k, Num p, Ord p)
  => HashPSQ k p v
  -> Map k (k, v)
  -> Map k p
  -> (k -> Bool)
  -> (k -> [(k, v)])
  -> (k -> p)
  -> (v -> v -> p)
  -> (p, [v])
astarMech queue paths gscore isGoal neighbours heuristic dist
  | Q.null queue = error "no solution found"
  | isGoal curKey = (gscore ! curKey, reconstructPath paths curKey)
  | otherwise =
    astarMech newQueue newPaths newGscore isGoal neighbours heuristic dist
  where
    (curKey, curPri, curVal, rest) = fromJust $ minView queue
    toVisit = neighbours curKey
    (newQueue, newGscore, newPaths) = foldl fscore (rest, gscore, paths) toVisit
    fscore (aQueue, scoreMap, pathMap) (aKey, aVal)
      | isNothing (M.lookup aKey scoreMap) || tentativeScore < scoreMap ! aKey =
        ( Q.insert aKey tentativeScore aVal aQueue
        , M.insert aKey tentativeScore scoreMap
        , M.insert aKey (curKey, curVal) pathMap)
      | otherwise = (aQueue, scoreMap, pathMap)
      where
        tentativeScore = fromJust (M.lookup curKey scoreMap) + dist curVal aVal

reconstructPath :: Ord k => Map k (k, v) -> k -> [v]
reconstructPath paths node
  | isNothing (M.lookup node paths) = []
  | otherwise = val : reconstructPath paths nextNode
  where
    (nextNode, val) = paths ! node
