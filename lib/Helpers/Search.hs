module Helpers.Search
  ( NodeFromVertex
  , VertexFromKey
  , astar
  , astarVal
  , bfs
  , bfsDist
  , bfsSafe
  , bfsSafeDist
  , dfs
  , dfsBest
  , dijkstraGoal
  , dijkstraGoalVal
  , dijkstraGoalValSafe
  , dijkstraAll
  , dijkstraMech
  , dijkstraUncertainGoalDist
  , dijkstraAllShortestPaths
  , findPattern
  , floydWarshall
  , floodFill
  , treeSize
  ) where

import qualified Data.Graph    as G (Edge, Graph, Tree (Node), Vertex, dfs,
                                     edges, graphFromEdges, vertices)
import           Data.Hashable (Hashable)
import           Data.HashPSQ  as Q (HashPSQ, insert, lookup, minView, null,
                                     singleton)
import           Data.List     as L (length)
import           Data.Map      as M (Map, alter, empty, insert, lookup, member,
                                     notMember, singleton, (!), keys, delete)
import           Data.Maybe    (fromJust, isNothing, mapMaybe)
import           Data.Sequence as Sq (Seq ((:<|), (:|>)), drop, length, null,
                                      singleton, takeWhileL, (!?))
import           Data.Set      as St (Set, empty, insert, member, notMember,
                                      singleton)

type NodeFromVertex node key = G.Vertex -> (node, key, [key])

type VertexFromKey a = a -> Maybe G.Vertex

-- Find all shortest paths in a graph
floydWarshall ::
     Ord node
  => (G.Graph, NodeFromVertex node key, VertexFromKey key)
  -> Map G.Edge Int
floydWarshall (graph, nodeFromVertex, _) = shortPaths valVertices distEdges
  where
    valVertices = G.vertices graph
    distVertices = foldl (\a b -> M.insert (b, b) 0 a) M.empty valVertices
    distEdges = foldl (\a b -> M.insert b 1 a) distVertices $ G.edges graph
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
bfsDist :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> Int
-- we need to reduce the distance by one because the path includes both the
-- starting point and the goal
bfsDist start neighbours =
  (+) (-1)
    . L.length
    . bfs (Sq.singleton start) (St.singleton start) M.empty neighbours

bfs :: Ord a => Seq a -> Set a -> Map a a -> (a -> [a]) -> (a -> Bool) -> [a]
bfs toSee seen paths neighbours isGoal
  | isNothing result = error "goal not found"
  | otherwise = fromJust result
  where
    result = bfsSafe toSee seen paths neighbours isGoal

bfsSafe ::
     Ord a
  => Seq a
  -> Set a
  -> Map a a
  -> (a -> [a])
  -> (a -> Bool)
  -> Maybe [a]
bfsSafe toSee seen paths neighbours isGoal
  | Sq.null toSee = Nothing
  | isGoal curPos = Just $ reconstructPath curPos paths
  | otherwise = bfsSafe toSeeNext newSeen newPaths neighbours isGoal
  where
    (curPos :<| rest) = toSee
    toConsider = filter (`St.notMember` seen) . neighbours $ curPos
    toSeeNext = foldl (:|>) rest toConsider
    newSeen = foldl (flip St.insert) seen toConsider
    newPaths = foldl (\a b -> M.insert b curPos a) paths toConsider

bfsSafeDist :: Ord a => a -> (a -> [a]) -> (a -> Bool) -> Maybe Int
-- we need to reduce the distance by one because the path includes both the
-- starting point and the goal
bfsSafeDist start neighbours isGoal =
  (+ (-1)) . L.length
    <$> bfsSafe
          (Sq.singleton start)
          (St.singleton start)
          M.empty
          neighbours
          isGoal

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
      checkBest
      $ St.insert node seen

-- Dijkstra
dijkstraGoalVal ::
     (Hashable k, Ord k, Show k, Show p, Num p, Ord p)
  => k
  -> p
  -> (k -> [(k, p)])
  -> k
  -> p
dijkstraGoalVal startKey startDist neighbours goal =
  fromJust . M.lookup goal . fst
    $ dijkstraGoal startKey startDist neighbours (== goal)

dijkstraGoalValSafe ::
     (Hashable k, Ord k, Show k, Show p, Num p, Ord p)
  => k
  -> p
  -> (k -> [(k, p)])
  -> k
  -> Maybe p
dijkstraGoalValSafe startKey startDist neighbours goal =
  M.lookup goal . fst $ dijkstraGoal startKey startDist neighbours (== goal)

dijkstraGoal ::
     (Hashable k, Ord k, Num p, Ord p)
  => k
  -> p
  -> (k -> [(k, p)])
  -> (k -> Bool)
  -> (Map k p, Map k k)
dijkstraGoal startKey startDist neighbours =
  snd
    . dijkstraMech
        (Q.singleton startKey startDist startKey)
        (M.singleton startKey startDist)
        M.empty
        neighbours

dijkstraUncertainGoalDist ::
     (Hashable k, Ord k, Show k, Num p, Ord p)
  => k
  -> p
  -> (k -> [(k, p)])
  -> (k -> Bool)
  -> p
dijkstraUncertainGoalDist startKey startDist neighbours isGoal =
  dists ! fromJust goal
  where
    (goal, (dists, _)) =
      dijkstraMech
        (Q.singleton startKey startDist startKey)
        (M.singleton startKey startDist)
        M.empty
        neighbours
        isGoal

dijkstraAll ::
     (Hashable k, Ord k, Num p, Ord p)
  => k
  -> p
  -> (k -> [(k, p)])
  -> (Map k p, Map k k)
dijkstraAll startKey startDist neighbours =
  dijkstraGoal startKey startDist neighbours (const False)

dijkstraMech ::
     (Hashable k, Ord k, Num p, Ord p)
  => HashPSQ k p k
  -> Map k p
  -> Map k k
  -> (k -> [(k, p)])
  -> (k -> Bool)
  -> (Maybe k, (Map k p, Map k k))
dijkstraMech queue dists paths neighbours isGoal
  | Q.null queue = (Nothing, (dists, paths))
  | isGoal curKey = (Just curKey, (dists, paths))
  | otherwise = dijkstraMech newQueue newDists newPaths neighbours isGoal
  where
    (curKey, estDist, _, rest) = fromJust (minView queue)
    toConsider = mapMaybe consider (neighbours curKey)
    newQueue = foldr (\(b, c) -> Q.insert b c b) rest toConsider
    newDists = foldr (uncurry M.insert) dists toConsider
    newPaths = foldr (\(b, _) -> M.insert b curKey) paths toConsider
    consider (aKey, anEdge)
      | not (M.member aKey dists) || estDist + anEdge < dists ! aKey =
        Just (aKey, estDist + anEdge)
      | otherwise = Nothing

dijkstraAllShortestPaths ::
     (Hashable k, Ord k, Num p, Ord p)
  => HashPSQ k p k
  -> Map k p
  -> Map k (Set k)
  -> (k -> [(k, p)])
  -> (k -> Bool)
  -> Map k (Set k)
dijkstraAllShortestPaths queue dists paths neighbours isGoal
  | isGoal node = prunedPaths
  | otherwise = dijkstraAllShortestPaths queue' dists' paths' neighbours isGoal
  where
    (node, estDist, _, rest) = fromJust . minView $ queue
    toConsider = mapMaybe consider . neighbours $ node
    queue' = foldr (\(b, c) -> Q.insert b c b) rest toConsider
    dists' = foldr (uncurry M.insert) dists toConsider
    paths' = foldr (M.alter (setUpdate node) . fst) paths toConsider
    setUpdate n Nothing   = Just . St.singleton $ n
    setUpdate n (Just ns) = Just . St.insert n $ ns
    consider (aNode, anEdge)
      | aNode `M.notMember` dists || estDist' <= dists M.! aNode =
        Just (aNode, estDist')
      | otherwise = Nothing
      where
        estDist' = estDist + anEdge
    prunedPaths = foldr M.delete paths . filter ((> bestDist) . (M.!) dists) $ goalNodes
    goalNodes = filter isGoal . keys $ dists
    bestDist = minimum . map (dists M.!) $ goalNodes

--A* search
astarVal ::
     (Hashable k, Ord k, Num p, Ord p, Show k, Show v)
  => k
  -> v
  -> (k -> Bool)
  -> (k -> [(k, v)])
  -> (k -> p)
  -> (v -> v -> p)
  -> p
astarVal startKey startValue isGoal neighbours heuristics dist =
  fst $ astar startKey startValue isGoal neighbours heuristics dist

-- queue: a priority queue, originally the start key priority value set
-- isGoal: returns True if the goal has been reached
-- neighbours: a function that returns a list of key  values of the nodes
--            accessible from the current node.
-- heuristics: the heuristics function
-- dist : a function returning the real distance between two values
astar ::
     (Hashable k, Ord k, Num p, Ord p, Show k)
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
     (Hashable k, Ord k, Num p, Ord p, Show k)
  => HashPSQ k p v
  -> Map k (k, v)
  -> Map k p
  -> (k -> Bool)
  -> (k -> [(k, v)])
  -> (k -> p)
  -> (v -> v -> p)
  -> (p, [v])
astarMech queue paths gscore isGoal neighbours heuristics dist
  | Q.null queue = error "no solution found"
  | isGoal node = (gscore ! node, reconstructPathAStar paths node)
  | otherwise =
    astarMech queue' paths' gscore' isGoal neighbours heuristics dist
  where
    (node, priority, val, rest) = fromJust $ minView queue
    toVisit = neighbours node
    (queue', gscore', paths') = foldr fscore (rest, gscore, paths) toVisit
    fscore (aNode, anEdge) (queue'', gscore'', paths'')
      | isNothing (M.lookup aNode gscore'') || tentativeScore < gscore'' ! aNode =
        ( Q.insert aNode hScore anEdge queue''
        , M.insert aNode tentativeScore gscore''
        , M.insert aNode (node, val) paths'')
      | otherwise = (queue'', gscore'', paths'')
      where
        tentativeScore = fromJust (M.lookup node gscore'') + dist val anEdge
        hScore = tentativeScore + heuristics aNode

reconstructPathAStar :: (Ord k, Show k) => Map k (k, v) -> k -> [v]
reconstructPathAStar paths node
  | isNothing (M.lookup node paths) = []
  | otherwise = val : reconstructPathAStar paths nextNode
  where
    (nextNode, val) = paths ! node

-- Patterns
-- | findPattern finds a pattern after *starting point*, with minimum size *min
-- size* in a Sequence (for efficiency), testing them with the provided boolean
-- test function and returns a pattern length.
findPattern :: Int -> Int -> (a -> a -> Bool) -> Seq a -> Int
findPattern startPoint minSize boolTest cycle
  | (startPoint + minSize) >= div (Sq.length cycle) 2 =
    error "Could not find pattern"
  | testPattern = potPatternLength
  | otherwise = findPattern startPoint (potPatternLength + 1) boolTest cycle
  where
    pruned = Sq.drop startPoint cycle
    (orVal :<| rest) = pruned
    potPatternLength =
      minSize
        + Sq.length (takeWhileL (not . boolTest orVal) $ Sq.drop minSize pruned)
    testPattern = boolTest orVal . fromJust $ pruned !? (2 * potPatternLength)

-- FloodFill
-- | floodFill returns a Forest of points reachable from  each of a list of
-- points corresponding to positions in a graph. Arguments are a list of nodes,
-- keys, edges and the list of points to consider.
floodFill :: Ord a => [(b, a, [a])] -> [a] -> [G.Tree G.Vertex]
floodFill edges points = G.dfs graph $ mapMaybe vfk points
  where
    (graph, _, vfk) = G.graphFromEdges edges

-- | treeSize returns the size of a Tree of Vertices
treeSize :: G.Tree G.Vertex -> Int
treeSize (G.Node a forest) = (1 +) . sum . map treeSize $ forest
