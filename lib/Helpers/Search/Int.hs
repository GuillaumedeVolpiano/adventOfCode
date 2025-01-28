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
  , travelingSalesman
  , travelingSalesmanNoReturn
  , IntLike
  , toInt
  , fromInt
  ) where

import           Data.Bits          (shiftL, (.&.))
import           Data.IntMap.Strict (IntMap, alter, assocs, empty,
                                     filterWithKey, keys, (!))
import qualified Data.IntMap.Strict as IM (delete, insert, member, notMember,
                                           singleton)
import           Data.IntPSQ        (IntPSQ, minView)
import qualified Data.IntPSQ        as Q (insert, null, singleton)
import           Data.IntSet        (IntSet, findMin, fromList, size, toList)
import qualified Data.IntSet        as S (delete, foldr, insert, member,
                                          notMember, singleton)
import           Data.List          (sortBy, subsequences)
import           Data.Map           (Map)
import qualified Data.Map           as M (fromList, insert, (!?))
import           Data.Maybe         (catMaybes, fromJust, isJust, isNothing,
                                     mapMaybe)
import           Data.Ord           (Down (..), comparing)
import           Data.Sequence      as Sq (Seq ((:<|), (:|>)))
import qualified Data.Sequence      as Sq (null, singleton)

-- | A class for types that are homomorphic with Ints or a subset of Ints.
class IntLike a where
  toInt :: a -> Int
  fromInt :: Int -> a

instance IntLike Int where
  toInt = id
  fromInt = id

bfsSafe ::
     IntLike a
  => Seq a
  -> IntSet
  -> IntMap Int
  -> (a -> [a])
  -> (a -> Bool)
  -> Maybe [a]
bfsSafe toSee seen paths neighbours isGoal
  | Sq.null toSee = Nothing
  | isGoal curPos = Just $ reconstructPath curPos paths
  | otherwise = bfsSafe toSee' seen' paths' neighbours isGoal
  where
    (curPos :<| rest) = toSee
    toConsider = filter (flip S.notMember seen . toInt) . neighbours $ curPos
    toSee' = foldr (flip (:|>)) rest toConsider
    seen' = foldr (S.insert . toInt) seen toConsider
    paths' = foldr (flip IM.insert (toInt curPos) . toInt) paths toConsider

bfsSafeDist :: IntLike a => a -> (a -> [a]) -> (a -> Bool) -> Maybe Int
-- we need to reduce the distance by one because the path includes both the
-- starting point and the goal
bfsSafeDist start neighbours isGoal =
  (+ (-1)) . length
    <$> bfsSafe
          (Sq.singleton start)
          (S.singleton . toInt $ start)
          empty
          neighbours
          isGoal

bfsAll :: IntLike a => Seq a -> IntMap Int -> (a -> [a]) -> IntMap Int
bfsAll toSee seen neighbours
  | Sq.null toSee = seen
  | otherwise = bfsAll toSee' seen' neighbours
  where
    (curPos :<| rest) = toSee
    toConsider = filter (flip IM.notMember seen . toInt) . neighbours $ curPos
    toSee' = foldl (:|>) rest toConsider
    dist = 1 + seen ! toInt curPos
    seen' = foldr (flip IM.insert dist . toInt) seen toConsider

reconstructPath :: IntLike a => a -> IntMap Int -> [a]
reconstructPath curPos paths
  | IM.notMember (toInt curPos) paths = [curPos]
  | otherwise = curPos : reconstructPath (fromInt $ paths ! toInt curPos) paths

dfs :: IntLike a => [a] -> (a -> [a]) -> IntSet -> IntSet
dfs [] _ seen = seen
dfs (node:ns) neighbours seen
  | S.member (toInt node) seen = dfs ns neighbours seen
  | otherwise =
    dfs (neighbours node ++ ns) neighbours $ S.insert (toInt node) seen

dfsDists :: IntLike a => [(a, Int)] -> (a -> [a]) -> IntMap Int -> IntMap Int
dfsDists [] _ seen = seen
dfsDists ((node, dist):ns) neighbours seen
  | IM.member (toInt node) seen = dfsDists ns neighbours seen
  | otherwise = dfsDists ns' neighbours seen'
  where
    toConsider = map (, dist + 1) . neighbours $ node
    ns' = toConsider ++ ns
    seen' = IM.insert (toInt node) dist seen

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
      | aNode `IM.notMember` dists || estDist + anEdge < dists ! aNode =
        Just (aNode, estDist + anEdge)
      | otherwise = Nothing

dijkstraUncertainGoalVal ::
     (Num p, Ord p) => Int -> p -> (Int -> [(Int, p)]) -> (Int -> Bool) -> p
dijkstraUncertainGoalVal node dist neighbours isGoal = dists ! goal
  where
    (Just goal, (dists, _)) =
      dijkstraMech
        (Q.singleton node dist ())
        (IM.singleton node dist)
        empty
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
    paths' = foldr (alter (update node) . fst) paths toConsider
    update p Nothing   = Just . S.singleton $ p
    update p (Just ps) = Just . S.insert p $ ps
    consider (aNode, anEdge)
      | aNode `IM.notMember` dists || estDist' <= dists ! aNode =
        Just (aNode, estDist')
      | otherwise = Nothing
      where
        estDist' = estDist + anEdge
    prunedPaths =
      foldr IM.delete paths . filter ((> bestDist) . (!) dists) $ goalNodes
    goalNodes = filter isGoal . keys $ dists
    bestDist = minimum . map (dists !) $ goalNodes

floydWarshall ::
     (Num a, Ord a) => [Int] -> (Int -> [(Int, a)]) -> Map (Int, Int) a
floydWarshall vertices neighbours = dists
  where
    dists = undefined
    initialMap = M.fromList . concat . foldr makeEdges [] $ vertices
    potEdges =
      [ (i, j, k)
      | i <- vertices
      , j <- vertices
      , i /= j
      , k <- vertices
      , i /= k
      , j /= k
      ]
    dists' = foldr fw initialMap potEdges
    makeEdges v =
      (:) (((v, v), 0) : (map (\(v', d) -> ((v, v'), d)) . neighbours $ v))
    fw (i, j, k) distMap
      | isNothing . checkDist (i, j, k) $ distMap = distMap
      | otherwise =
        M.insert (i, j) (fromJust . checkDist (i, j, k) $ distMap) distMap
    checkDist (i, j, k) distMap
      | isNothing (distSum (i, j, k) distMap)
          || (isJust (distMap M.!? (i, j))
                && distMap M.!? (i, j) <= distSum (i, j, k) distMap) = Nothing
      | otherwise = distSum (i, j, k) distMap
    distSum (i, j, k) distMap =
      (+) <$> distMap M.!? (i, k) <*> distMap M.!? (k, j)

travelingSalesmanNoReturn :: Int -> IntMap Int -> IntMap Int
travelingSalesmanNoReturn numBits edges =
  filterWithKey (\k _ -> k > encodedSize) findSubsets
  where
    bitSize = shiftL 1 numBits
    encodedSize = shiftL 1 (numBits * maxNode)
    maxNode = maximum . map (.&. (bitSize - 1)) . keys $ edges
    pois =
      map fromList . sortBy (comparing (Down . length)) . tail . subsequences
        $ [1 .. maxNode]
    findSubsets = foldr mapSubset empty pois
    mapSubset set subsets
      | size set == 1 =
        IM.insert (encode set + findMin set) (edges ! findMin set) subsets
      | otherwise = S.foldr (subsetise set) subsets set
    subsetise set k subsests =
      IM.insert
        (encode set + k)
        (bestSub subsests k . S.delete k $ set)
        subsests
    bestSub bests k set =
      minimum
        [bests ! (encode set + m) + edges ! edgeCode m k | m <- toList set]
    edgeCode m k = mMK + shiftL mmk numBits
      where
        mmk = min m k
        mMK = max m k
    encode = flip shiftL numBits . S.foldr (\a b -> a + shiftL b numBits) 0

travelingSalesman :: Int -> IntMap Int -> Int
travelingSalesman numBits edges =
  minimum . map finish . assocs . travelingSalesmanNoReturn numBits $ edges
  where
    bitSize = shiftL 1 numBits
    finish (k, d) = d + edges ! (k .&. (bitSize - 1))
