module Search
  ( NodeFromVertex
  , VertexFromKey
  , astar
  , astarVal
  , bfsDist
  , dfs
  , dfsBest
  , dijkstraGoal
  , dijkstraGoalVal
  , dijkstraAll
  , floydWarshall
  ) where

import           Data.Graph    (Edge, Graph, Vertex, edges, vertices)
import           Data.Hashable (Hashable)
import           Data.HashPSQ  as Q (HashPSQ, insert, lookup, minView, null,
                                     singleton)
import           Data.Map      as M (Map, empty, insert, lookup, member,
                                     notMember, singleton, (!))
import           Data.Maybe    (fromJust, isNothing, mapMaybe)
import           Data.Sequence as Sq (Seq ((:<|), (:|>)), null, singleton)
import           Data.Set      as St (Set, empty, insert, member, notMember,
                                      singleton)

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
      | M.notMember (y, z) dists =
        path x y zs $ M.insert (y, z) (dists ! (y, x) + dists ! (x, z)) dists
      | dists ! (y, z) > dists ! (y, x) + dists ! (x, z) =
        path x y zs $ M.insert (y, z) (dists ! (y, x) + dists ! (x, z)) dists
      | otherwise = path x y zs dists
    testMemberships x y z dists = M.member (y, x) dists && M.member (x, z) dists

-- Breadth first search
bfsDist :: Ord a => a -> a -> (a -> [a]) -> (a -> Bool) -> Int
bfsDist start goal neighbours =
  length . bfsMech (Sq.singleton start) (St.singleton goal) M.empty neighbours

bfsMech ::
     Ord a => Seq a -> Set a -> Map a a -> (a -> [a]) -> (a -> Bool) -> [a]
bfsMech toSee seen paths neighbours isGoal
  | Sq.null toSee = error "goal not found"
  | isGoal curPos = reconstructPath curPos paths
  | otherwise = bfsMech toSeeNext newSeen newPaths neighbours isGoal
  where
    (curPos :<| rest) = toSee
    toConsider = filter (`St.notMember` seen) . neighbours $ curPos
    toSeeNext = foldl (:|>) rest toConsider
    newSeen = foldl (flip St.insert) seen toConsider
    newPaths = foldl (\a b -> M.insert b curPos a) paths toConsider

reconstructPath :: Ord a => a -> Map a a -> [a]
reconstructPath curPos paths
  | M.notMember curPos paths = [curPos]
  | otherwise = curPos : reconstructPath (paths M.! curPos) paths

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

-- Dijkstra
dijkstraGoalVal ::
     (Hashable k, Ord k, Show k, Num p, Ord p)
  => k
  -> p
  -> (k -> [(k, p)])
  -> k
  -> p
dijkstraGoalVal startKey startDist neighbours goal =
  fromJust . M.lookup goal . fst $
  dijkstraGoal startKey startDist neighbours (== goal)

dijkstraGoal ::
     (Hashable k, Ord k, Show k, Num p, Ord p)
  => k
  -> p
  -> (k -> [(k, p)])
  -> (k -> Bool)
  -> (Map k p, Map k k)
dijkstraGoal startKey startDist =
  dijkstraMech
    (Q.singleton startKey startDist startKey)
    (M.singleton startKey startDist)
    M.empty

dijkstraAll ::
     (Hashable k, Ord k, Show k, Num p, Ord p)
  => k
  -> p
  -> (k -> [(k, p)])
  -> (Map k p, Map k k)
dijkstraAll startKey startDist neighbours =
  dijkstraGoal startKey startDist neighbours (const False)

dijkstraMech ::
     (Hashable k, Ord k, Show k, Num p, Ord p)
  => HashPSQ k p k
  -> Map k p
  -> Map k k
  -> (k -> [(k, p)])
  -> (k -> Bool)
  -> (Map k p, Map k k)
dijkstraMech queue dists paths neighbours isGoal
  | Q.null queue || isGoal curKey = (dists, paths)
  | otherwise = dijkstraMech newQueue newDists newPaths neighbours isGoal
  where
    (curKey, estDist, _, rest) = fromJust (minView queue)
    toConsider = mapMaybe consider (neighbours curKey)
    newQueue = foldl (\a (b, c) -> Q.insert b c b a) rest toConsider
    newDists = foldl (\a (b, c) -> M.insert b c a) dists toConsider
    newPaths = foldl (\a (b, _) -> M.insert b curKey a) paths toConsider
    consider (aKey, anEdge)
      | not (M.member aKey dists) || estDist + anEdge < dists ! aKey =
        Just (aKey, estDist + anEdge)
      | otherwise = Nothing

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
  | isGoal curKey = (gscore ! curKey, reconstructPathAStar paths curKey)
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

reconstructPathAStar :: Ord k => Map k (k, v) -> k -> [v]
reconstructPathAStar paths node
  | isNothing (M.lookup node paths) = []
  | otherwise = val : reconstructPathAStar paths nextNode
  where
    (nextNode, val) = paths ! node
