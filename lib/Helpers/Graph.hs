{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Helpers.Graph
  ( assocsToGraph
  , assocsToReverseGraph
  , assocsToDigraph
  , dicToGraph
  , graphToViz
  , Pos
  , east
  , west
  , north
  , south
  , dirs
  , neighbours
  ) where

import           Data.Array.IArray                 (IArray)
import           Data.Array.Unboxed                (UArray, bounds, inRange)
import           Data.Graph.Inductive.Graph        (Graph, LEdge, LNode, Node,
                                                    mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     (DotGraph, Labellable,
                                                    graphToDot, printDotGraph,
                                                    quickParams)
import           Data.List                         (concatMap, nub)
import           Data.Map                          (Map, assocs, elems,
                                                    fromList, keys, (!))
import           Data.Text.Lazy                    (Text)
import           Linear.V2                         (V2 (..))

type Pos = V2 Int

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

dirs = [north, south, east, west]

dicToGraph :: Map String [(String, a)] -> Gr String a
dicToGraph = assocsToGraph . assocs

assocsToGraph :: [(String, [(String, a)])] -> Gr String a
assocsToGraph ass = mkGraph n (edges n ass)
  where
    n = nodes ass

assocsToReverseGraph :: [(String, [(String, a)])] -> Gr String a
assocsToReverseGraph ass = mkGraph n (reverseEdges n ass)
  where
    n = nodes ass

assocsToDigraph :: (Eq a) => [(String, [(String, a)])] -> Gr String a
assocsToDigraph ass = mkGraph n diEdges
  where
    n = nodes ass
    diEdges = nub $ edges n ass ++ reverseEdges n ass

nodes :: [(String, [(String, a)])] -> [LNode String]
nodes ass = zip [0 ..] . nub $ map fst ass ++ map fst (concatMap snd ass)

edges :: [LNode String] -> [(String, [(String, a)])] -> [LEdge a]
edges n =
  map (\(a, (b, c)) -> (labelToNode ! a, labelToNode ! b, c)) .
  concatMap (\(a, b) -> map (a, ) b)
  where
    labelToNode = fromList . map (\(a, b) -> (b, a)) $ n

reverseEdges :: [LNode String] -> [(String, [(String, a)])] -> [LEdge a]
reverseEdges n =
  map (\(a, (b, c)) -> (labelToNode ! b, labelToNode ! a, c)) .
  concatMap (\(a, b) -> map (a, ) b)
  where
    labelToNode = fromList . map (\(a, b) -> (b, a)) $ n

graphToViz :: Gr String String -> Text
graphToViz = printDotGraph . graphToDot quickParams

neighbours :: (IArray UArray a) => UArray Pos a -> Pos -> [Pos]
neighbours a p = filter (inRange b) . map (p +) $ dirs
  where
    b = bounds a
