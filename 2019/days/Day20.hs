{-# LANGUAGE TupleSections #-}

module Day20
  ( part1
  , part2
  ) where

import           Control.Lens.Getter           ((^.))
import           Data.Array.Unboxed            as A (UArray, bounds, indices,
                                                     (!))
import           Data.Bifunctor                (first, second)
import           Data.Char                     (isUpper)
import           Data.Graph.Inductive.Graph    (labEdges, labNodes)
import           Data.Graph.Inductive.Query.SP (spLength)
import           Data.HashMap.Lazy             as M (HashMap, fromList, keys,
                                                     toList, (!))
import           Data.HashSet                  as St (fromList, member,
                                                      singleton)
import           Data.List                     (groupBy, nub, sortBy, tails,
                                                unfoldr)
import           Data.Maybe                    (Maybe (Just, Nothing), fromJust,
                                                isJust, mapMaybe)
import           Data.Sequence                 as Sq (singleton)
import           Helpers.Graph                 (Gr, Pos, assocsToDigraph, dirs,
                                                east, neighbours, south,
                                                unfoldAssocs)
import           Helpers.Parsers               (arrayFromString)
import           Helpers.Search                (dijkstraGoalVal)
import           Linear.V2                     (V2 (..))
import           Linear.V3                     (V3 (..), _xy, _z)

type Maze = UArray Pos Char

type Portals = HashMap Pos Pos

type Start = Pos

type End = Pos

type RecPos = V3 Int

largeGraphBuilder :: [Pos] -> Maze -> Gr Pos Int
largeGraphBuilder posList maze =
  assocsToDigraph .
  concatMap
    (\p -> unfoldr (unfoldAssocs toCons id) (Sq.singleton p, St.singleton p)) $
  posList
  where
    toCons set =
      map (, 1) .
      filter (\p -> maze A.! p == '.' && not (p `member` set)) . neighbours maze

portalGraphBuilder :: [Pos] -> [(Pos, Pos)] -> Gr Pos Int -> Gr Pos Int
portalGraphBuilder positions portals largeGraph =
  assocsToDigraph $ distPort ++ pairs
  where
    lnodes = labNodes largeGraph
    places = filter ((`elem` positions) . snd) lnodes
    placesDict = M.fromList places
    distPort = map (\(a, b) -> (a, [(b, 1)])) portals
    pairs =
      filter (not . null . snd) .
      zipWith
        (\a b ->
           ( placesDict M.! a
           , map (second fromJust) .
             filter (isJust . snd) .
             map (\c -> (placesDict M.! c, spLength a c largeGraph)) .
             filter (/= a) $
             b))
        (init . map fst $ places) .
      tails . map fst $
      places

findPortals :: Maze -> (Start, End, Portals)
findPortals maze = (start, end, portals)
  where
    rawPortals = filter (isUpper . (A.!) maze) . indices $ maze
    mapped =
      map (foldr (\(a, b) (_, d) -> (a, b : d)) ("", [])) .
      groupBy (\(a, _) (b, _) -> a == b) .
      sortBy (\(a, _) (b, _) -> compare a b) .
      map
        ((\(a, b) ->
            ( [maze A.! a, maze A.! b]
            , head . filter ((== '.') . (A.!) maze) $
              neighbours maze a ++ neighbours maze b)) .
         (\p -> (p, head . filter (`elem` rawPortals) $ [p + east, p + south]))) $
      filter
        (\p -> p + east `elem` rawPortals || p + south `elem` rawPortals)
        rawPortals
    start = head . snd . head . filter ((== "AA") . fst) $ mapped
    end = head . snd . head . filter ((== "ZZ") . fst) $ mapped
    portals =
      M.fromList .
      concatMap ((\[a, b] -> [(a, b), (b, a)]) . snd) .
      filter (not . flip elem ["AA", "ZZ"] . fst) $
      mapped

findShortest :: Maze -> Int
findShortest maze = fromJust . spLength startNode endNode $ portalGraph
  where
    (start, end, portals) = findPortals maze
    places = start : end : keys portals
    largeGraph = largeGraphBuilder places maze
    portalGraph = portalGraphBuilder places (toList portals) largeGraph
    startNode = fst . head . filter ((== start) . snd) . labNodes $ portalGraph
    endNode = fst . head . filter ((== end) . snd) . labNodes $ portalGraph

spatialise :: Int -> V2 Int -> V3 Int
spatialise z (V2 x y) = V3 x y z

findRecursiveShortest :: Maze -> Int
findRecursiveShortest maze = dijkstraGoalVal recStart 0 neighb recEnd
  where
    (start, end, portals) = findPortals maze
    places = start : end : keys portals
    largeGraph = largeGraphBuilder places maze
    portalGraph = portalGraphBuilder places (toList portals) largeGraph
    nodeDict = M.fromList . labNodes $ portalGraph
    edges =
      M.fromList .
      map (foldr (\(a, b) (_, c) -> (a, b : c)) (south, [])) .
      groupBy (\(a, _) (b, _) -> a == b) .
      sortBy (\(a, _) (b, _) -> compare a b) .
      map (\(a, b, c) -> (nodeDict M.! a, (nodeDict M.! b, c))) . labEdges $
      portalGraph
    recStart = spatialise 0 start
    recEnd = spatialise 0 end
    (V2 mx my, V2 mX mY) = bounds maze
    external (V3 x y _) =
      x == mx + 2 || x == mX - 2 || y == my + 2 || y == mY - 2
    neighb p
      | p ^. _xy == start && p ^. _z /= 0 || p ^. _xy == end = []
      | otherwise =
        mapMaybe (recur . first (spatialise (p ^. _z))) $ edges M.! (p ^. _xy)
    recur pair@(p@(V3 x y z), dist)
      | dist /= 1 = Just pair
      | external p = Just (V3 x y (z + 1), 1)
      | not (external p) && z > 0 = Just (V3 x y (z - 1), 1)
      | otherwise = Nothing

part1 :: Bool -> String -> String
part1 _ = show . findShortest . arrayFromString

part2 :: Bool -> String -> String
part2 _ = show . findRecursiveShortest . arrayFromString
