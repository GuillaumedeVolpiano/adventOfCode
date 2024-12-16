module Helpers.Search.Int
  ( dijkstraMech
  , dijkstraUncertainGoalVal
  , dijkstraAllShortestPaths
  ) where

import           Data.IntMap as M (IntMap, alter, delete, empty, insert, keys,
                                   member, notMember, singleton, (!))
import           Data.IntPSQ as Q (IntPSQ, insert, minView, null, singleton)
import           Data.IntSet as S (IntSet, insert, singleton)
import           Data.Maybe  (fromJust, mapMaybe)

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
      | aNode `notMember` dists || estDist + anEdge < dists M.! aNode =
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
  | Q.null queue = prunedPaths
  | isGoal reindeer =
    dijkstraAllShortestPaths rest dists paths neighbours isGoal
  | otherwise = dijkstraAllShortestPaths queue' dists' paths' neighbours isGoal
  where
    (reindeer, estDist, _, rest) = fromJust . minView $ queue
    toConsider = mapMaybe consider . neighbours $ reindeer
    queue' = foldr (\(b, c) -> Q.insert b c ()) rest toConsider
    dists' = foldr (uncurry M.insert) dists toConsider
    paths' = foldr (M.alter (update reindeer) . fst) paths toConsider
    update p Nothing   = Just . S.singleton $ p
    update p (Just ps) = Just . S.insert p $ ps
    consider (aNode, anEdge)
      | aNode `notMember` dists || estDist' <= dists M.! aNode =
        Just (aNode, estDist')
      | otherwise = Nothing
      where
        estDist' = estDist + anEdge
    prunedPaths =
      foldr M.delete paths . filter ((> bestDist) . (M.!) dists) $ goalNodes
    goalNodes = filter isGoal . keys $ dists
    bestDist = minimum . map (dists M.!) $ goalNodes
