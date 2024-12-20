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

import           Data.IntMap   as M (IntMap, alter, delete, empty, insert, keys,
                                     member, notMember, singleton, (!))
import           Data.IntPSQ   as Q (IntPSQ, insert, minView, null, singleton)
import           Data.IntSet   as S (IntSet, insert, member, notMember,
                                     singleton)
import           Data.List     as L (length)
import           Data.Maybe    (fromJust, mapMaybe)
import           Data.Sequence as Sq (Seq ((:<|), (:|>)), null, singleton)

import           Debug.Trace

bfsSafe ::
     Seq Int
  -> IntSet
  -> IntMap Int
  -> (Int -> [Int])
  -> (Int -> Bool)
  -> Maybe [Int]
bfsSafe toSee seen paths neighbours isGoal
  | Sq.null toSee = Nothing
  | isGoal curPos = Just $ reconstructPath curPos paths
  | otherwise = bfsSafe toSee' seen' paths' neighbours isGoal
  where
    (curPos :<| rest) = toSee
    toConsider = filter (`S.notMember` seen) . neighbours $ curPos
    toSee' = foldl (:|>) rest toConsider
    seen' = foldl (flip S.insert) seen toConsider
    paths' = foldl (\a b -> M.insert b curPos a) paths toConsider

bfsSafeDist :: Int -> (Int -> [Int]) -> (Int -> Bool) -> Maybe Int
-- we need to reduce the distance by one because the path includes both the
-- starting point and the goal
bfsSafeDist start neighbours isGoal =
  (+ (-1)) . L.length
    <$> bfsSafe
          (Sq.singleton start)
          (S.singleton start)
          M.empty
          neighbours
          isGoal

bfsAll :: Seq Int -> IntMap Int -> (Int -> [Int]) -> IntMap Int
bfsAll toSee seen neighbours
  | Sq.null toSee = seen
  | otherwise =
      bfsAll
      toSee'
      seen'
      neighbours
  where
    (curPos :<| rest) = toSee
    toConsider = filter (`M.notMember` seen) . neighbours $ curPos
    toSee' = foldl (:|>) rest toConsider
    dist = 1 + seen ! curPos
    seen' = foldr (`M.insert` dist) seen toConsider

reconstructPath :: Int -> IntMap Int -> [Int]
reconstructPath curPos paths
  | M.notMember curPos paths = [curPos]
  | otherwise = curPos : reconstructPath (paths M.! curPos) paths

dfs :: [Int] -> (Int -> [Int]) -> IntSet -> IntSet
dfs [] _ seen = seen
dfs (node:ns) neighbours seen
  | S.member node seen = dfs ns neighbours seen
  | otherwise = dfs (neighbours node ++ ns) neighbours $ S.insert node seen

dfsDists :: [(Int, Int)] -> (Int -> [Int]) -> IntMap Int -> IntMap Int
dfsDists [] _ seen = seen
dfsDists ((node, dist):ns) neighbours seen
  | M.member node seen = dfsDists ns neighbours seen
  | otherwise = dfsDists ns' neighbours seen'
  where
    toConsider = map (, dist + 1) . neighbours $ node
    ns' = toConsider ++ ns
    seen' = M.insert node dist seen

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
    dists' = foldr (uncurry M.insert) dists toConsider
    paths' = foldr (flip M.insert node . fst) paths toConsider
    consider (aNode, anEdge)
      | aNode `M.notMember` dists || estDist + anEdge < dists M.! aNode =
        Just (aNode, estDist + anEdge)
      | otherwise = Nothing

dijkstraUncertainGoalVal ::
     (Num p, Ord p) => Int -> p -> (Int -> [(Int, p)]) -> (Int -> Bool) -> p
dijkstraUncertainGoalVal node dist neighbours isGoal = dists M.! goal
  where
    (Just goal, (dists, _)) =
      dijkstraMech
        (Q.singleton node dist ())
        (M.singleton node dist)
        M.empty
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
    dists' = foldr (uncurry M.insert) dists toConsider
    paths' = foldr (M.alter (update node) . fst) paths toConsider
    update p Nothing   = Just . S.singleton $ p
    update p (Just ps) = Just . S.insert p $ ps
    consider (aNode, anEdge)
      | aNode `M.notMember` dists || estDist' <= dists M.! aNode =
        Just (aNode, estDist')
      | otherwise = Nothing
      where
        estDist' = estDist + anEdge
    prunedPaths =
      foldr M.delete paths . filter ((> bestDist) . (M.!) dists) $ goalNodes
    goalNodes = filter isGoal . keys $ dists
    bestDist = minimum . map (dists M.!) $ goalNodes
