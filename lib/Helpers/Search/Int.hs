{-# LANGUAGE TupleSections #-}

module Helpers.Search.Int
  ( dfs
  , dfsDists
  , bfsAll
  , bfsSafe
  , bfsSafeDist
  , dijkstraMech
  , dijkstraUncertainGoalVal
  , dijkstraAllShortestPaths
  ) where

import           Data.IntMap   as IM (IntMap, alter, delete, empty, insert, keys,
                                     member, notMember, singleton, (!))
import           Data.IntPSQ   as Q (IntPSQ, insert, minView, null, singleton)
import           Data.IntSet   as S (IntSet, insert, member, notMember,
                                     singleton)
import           Data.List     as L (length)
import           Data.Maybe    (fromJust, mapMaybe, catMaybes, isNothing, isJust)
import           Data.Sequence as Sq (Seq ((:<|), (:|>)), null, singleton)
import Data.Map (Map)
import qualified Data.Map as M (fromList, insert, (!?))

bfsSafe ::
     Seq Int -> IntMap Int -> (Int -> [Int]) -> (Int -> Bool) -> Maybe [Int]
bfsSafe toSee paths neighbours isGoal
  | Sq.null toSee = Nothing
  | isGoal curPos = Just $ reconstructPath curPos paths
  | otherwise = bfsSafe toSee' paths' neighbours isGoal
  where
    (curPos :<| rest) = toSee
    toConsider = filter (`IM.notMember` paths) . neighbours $ curPos
    toSee' = foldl (:|>) rest toConsider
    paths' = foldl (\a b -> IM.insert b curPos a) paths toConsider

bfsSafeDist :: Int -> (Int -> [Int]) -> (Int -> Bool) -> Maybe Int
-- we need to reduce the distance by one because the path includes both the
-- starting point and the goal
bfsSafeDist start neighbours isGoal =
  (+ (-1)) . L.length <$> bfsSafe (Sq.singleton start) IM.empty neighbours isGoal

bfsAll :: Seq Int -> IntMap Int -> (Int -> [Int]) -> IntMap Int
bfsAll toSee seen neighbours
  | Sq.null toSee = seen
  | otherwise = bfsAll toSee' seen' neighbours
  where
    (curPos :<| rest) = toSee
    toConsider = filter (`IM.notMember` seen) . neighbours $ curPos
    toSee' = foldl (:|>) rest toConsider
    dist = 1 + seen ! curPos
    seen' = foldr (`IM.insert` dist) seen toConsider

reconstructPath :: Int -> IntMap Int -> [Int]
reconstructPath curPos paths
  | IM.notMember curPos paths = [curPos]
  | otherwise = curPos : reconstructPath (paths IM.! curPos) paths

dfs :: [Int] -> (Int -> [Int]) -> IntSet -> IntSet
dfs [] _ seen = seen
dfs (node:ns) neighbours seen
  | S.member node seen = dfs ns neighbours seen
  | otherwise = dfs (neighbours node ++ ns) neighbours $ S.insert node seen

dfsDists :: [(Int, Int)] -> (Int -> [Int]) -> IntMap Int -> IntMap Int
dfsDists [] _ seen = seen
dfsDists ((node, dist):ns) neighbours seen
  | IM.member node seen = dfsDists ns neighbours seen
  | otherwise = dfsDists ns' neighbours seen'
  where
    toConsider = map (, dist + 1) . neighbours $ node
    ns' = toConsider ++ ns
    seen' = IM.insert node dist seen

dijkstraMech ::
     (Num p, Ord p)
  => IntPSQ p ()
  -> IntMap p
  -> IntMap Int
  -> (Int -> [(Int, p)])
  -> (Int -> Bool)
  -> (Maybe Int, (IntMap p, IntMap Int))
dijkstraMech queue dists paths neighbours isGoal
  | Q.null queue = (Nothing, (dists, paths))
  | isGoal node = (Just node, (dists, paths))
  | otherwise = dijkstraMech queue' dists' paths' neighbours isGoal
  where
    (node, estDist, _, rest) = fromJust (minView queue)
    toConsider = mapMaybe consider (neighbours node)
    queue' = foldr (\(b, c) -> Q.insert b c ()) rest toConsider
    dists' = foldr (uncurry IM.insert) dists toConsider
    paths' = foldr (flip IM.insert node . fst) paths toConsider
    consider (aNode, anEdge)
      | aNode `IM.notMember` dists || estDist + anEdge < dists IM.! aNode =
        Just (aNode, estDist + anEdge)
      | otherwise = Nothing

dijkstraUncertainGoalVal ::
     (Num p, Ord p) => Int -> p -> (Int -> [(Int, p)]) -> (Int -> Bool) -> p
dijkstraUncertainGoalVal node dist neighbours isGoal = dists IM.! goal
  where
    (Just goal, (dists, _)) =
      dijkstraMech
        (Q.singleton node dist ())
        (IM.singleton node dist)
        IM.empty
        neighbours
        isGoal

dijkstraAllShortestPaths ::
     (Num p, Ord p)
  => IntPSQ p ()
  -> IntMap p
  -> IntMap IntSet
  -> (Int -> [(Int, p)])
  -> (Int -> Bool)
  -> IntMap IntSet
dijkstraAllShortestPaths queue dists paths neighbours isGoal
  | isGoal node = prunedPaths
  | otherwise = dijkstraAllShortestPaths queue' dists' paths' neighbours isGoal
  where
    (node, estDist, _, rest) = fromJust . minView $ queue
    toConsider = mapMaybe consider . neighbours $ node
    queue' = foldr (\(b, c) -> Q.insert b c ()) rest toConsider
    dists' = foldr (uncurry IM.insert) dists toConsider
    paths' = foldr (IM.alter (update node) . fst) paths toConsider
    update p Nothing   = Just . S.singleton $ p
    update p (Just ps) = Just . S.insert p $ ps
    consider (aNode, anEdge)
      | aNode `IM.notMember` dists || estDist' <= dists IM.! aNode =
        Just (aNode, estDist')
      | otherwise = Nothing
      where
        estDist' = estDist + anEdge
    prunedPaths =
      foldr IM.delete paths . filter ((> bestDist) . (IM.!) dists) $ goalNodes
    goalNodes = filter isGoal . keys $ dists
    bestDist = minimum . map (dists IM.!) $ goalNodes

floydWarshall :: (Num a, Ord a) => [Int] -> (Int -> [(Int, a)]) -> Map (Int, Int) a
floydWarshall vertices neighbours = dists
  where
    dists = undefined
    initialMap = M.fromList . concat . foldr makeEdges [] $ vertices
    potEdges = [(i, j, k) | i <- vertices, j <- vertices, i /= j, k <- vertices, i /= k, j /=k]
    dists' = foldr fw initialMap potEdges
    makeEdges v = (:) (((v, v), 0) : (map (\(v', d) -> ((v, v'), d)) . neighbours $ v))
    fw (i, j, k) distMap 
      | isNothing . checkDist (i, j, k) $ distMap = distMap
      | otherwise = M.insert (i, j) (fromJust . checkDist (i, j, k) $ distMap) distMap
    checkDist (i, j, k) distMap
      | isNothing (distSum (i, j, k) distMap) || (isJust (distMap M.!? (i, j)) && distMap M.!? (i, j) <= distSum (i, j, k) distMap) = Nothing
      | otherwise = distSum (i, j, k) distMap
    distSum (i, j, k) distMap = (+) <$> distMap M.!? (i, k) <*> distMap M.!? (k, j)
