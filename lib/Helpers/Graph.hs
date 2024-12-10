{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Helpers.Graph
  ( Gr
  , assocsToMapGraph
  , assocsToGraph
  , assocsToReverseGraph
  , assocsToDigraph
  , dicToGraph
  , graphToViz
  , Pos
  , east
  , west
  , north
  , origin
  , south
  , dirs
  , neighbours
  , unfoldAssocs
  , left
  , right
  , manhattanDistance
  ) where

import           Data.Array.IArray                 (IArray)
import           Data.Array.Unboxed                (UArray, bounds, inRange)
import           Data.Bifunctor                    (first)
import           Data.Graph.Inductive.Graph        (Graph, LEdge, LNode, Node,
                                                    mkGraph)
import           Data.Graph.Inductive.NodeMap      (NodeMap, mkMapGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     (DotGraph, Labellable,
                                                    graphToDot, printDotGraph,
                                                    quickParams)
import           Data.Hashable                     (Hashable)
import           Data.HashSet                      (HashSet, insert, member)
import           Data.List                         (concatMap, nub)
import           Data.Map                          (Map, assocs, elems,
                                                    fromList, keys, (!))
import           Data.Sequence                     (Seq ((:|>)),
                                                    ViewL (EmptyL, (:<)), viewl)
import           Data.Text.Lazy                    (Text)
import           Linear.V2                         (V2 (..))

type Pos = V2 Int

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

origin :: V2 Int
origin = V2 0 0

dirs = [north, south, east, west]

dicToGraph :: (Eq a, Ord a) => Map a [(a, b)] -> Gr a b
dicToGraph = assocsToGraph . assocs

assocsToMapGraph :: (Eq a, Ord a) => [(a, [(a, b)])] -> (Gr a b, NodeMap a)
assocsToMapGraph ass = mkMapGraph n edges
  where
    n = nub $ map fst ass ++ map fst (concatMap snd ass)
    edges =
      concatMap
        (\(lnode, ledges) ->
           map (\(lnode', ledge) -> (lnode, lnode', ledge)) ledges)
        ass

assocsToGraph :: (Eq a, Ord a) => [(a, [(a, b)])] -> Gr a b
assocsToGraph ass = mkGraph n (edges n ass)
  where
    n = nodes ass

assocsToReverseGraph :: (Eq a, Ord a) => [(a, [(a, b)])] -> Gr a b
assocsToReverseGraph ass = mkGraph n (reverseEdges n ass)
  where
    n = nodes ass

assocsToDigraph :: (Eq a, Ord a) => (Eq b) => [(a, [(a, b)])] -> Gr a b
assocsToDigraph ass = mkGraph n diEdges
  where
    n = nodes ass
    diEdges = nub $ edges n ass ++ reverseEdges n ass

nodes :: (Eq a) => [(a, [(a, b)])] -> [LNode a]
nodes ass = zip [0 ..] . nub $ map fst ass ++ map fst (concatMap snd ass)

edges :: (Ord a) => [LNode a] -> [(a, [(a, b)])] -> [LEdge b]
edges n =
  map (\(a, (b, c)) -> (labelToNode ! a, labelToNode ! b, c))
    . concatMap (\(a, b) -> map (a, ) b)
  where
    labelToNode = fromList . map (\(a, b) -> (b, a)) $ n

reverseEdges :: (Ord a) => [LNode a] -> [(a, [(a, b)])] -> [LEdge b]
reverseEdges n =
  map (\(a, (b, c)) -> (labelToNode ! b, labelToNode ! a, c))
    . concatMap (\(a, b) -> map (a, ) b)
  where
    labelToNode = fromList . map (\(a, b) -> (b, a)) $ n

graphToViz :: Gr String String -> Text
graphToViz = printDotGraph . graphToDot quickParams

neighbours :: (IArray UArray a) => UArray Pos a -> Pos -> [Pos]
neighbours a p = filter (inRange b) . map (p +) $ dirs
  where
    b = bounds a

-- this function unfoldr·s a pair made of a sequence and a hashset (usually a
-- singleton) into a list that can be plugged into one of the assocsTo
-- functions. The produced Graph will be of type Gr b c. The additional arguments
-- it takes are a function that can transform an element of the sequence in a
-- list of pairs of sequence elements and edge values based on the set of
-- node labels already seen (HashSet b -> a -> [(a, c)]), and a function that
-- can transform a sequence element into a set element (a -> b)
unfoldAssocs ::
     (Eq a, Hashable b)
  => (HashSet b -> a -> [(a, c)])
  -> (a -> b)
  -> (Seq a, HashSet b)
  -> Maybe ((b, [(b, c)]), (Seq a, HashSet b))
unfoldAssocs toConsider see (toSee, seen)
  | decons == EmptyL = Nothing
  | otherwise =
    Just ((see curVal, map (first see) consNext), (newToSee, newSeen))
  where
    decons = viewl toSee
    (curVal :< rest) = decons
    consNext = toConsider seen curVal
    newToSee = foldr (flip (:|>) . fst) rest consNext
    newSeen = foldr (insert . see . fst) seen consNext

right :: Pos -> Pos
right (V2 x y) = V2 (-y) x

left :: Pos -> Pos
left (V2 x y) = V2 y (-x)

manhattanDistance :: Pos -> Pos -> Int
manhattanDistance (V2 x0 y0) (V2 x1 y1) = abs (x0 - x1) + abs (y0 - y1)
