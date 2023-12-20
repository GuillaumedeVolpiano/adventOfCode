{-# LANGUAGE TupleSections #-}

module Helpers.Graph (dicToGraph, graphToViz) where

import           Data.Graph.Inductive.Graph        (Graph, Node, mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     (DotGraph, graphToDot,
                                                    nonClusteredParams)
import           Data.Map                          (Map, assocs, fromList, keys,
                                                    (!))

dicToGraph :: (Ord a) => Map a [a] -> Gr a (a, a)
dicToGraph dic = mkGraph nodes edges
  where
    nodes = zip [0 ..] . keys $ dic
    edges =
      map (\(a, b) -> (labelToNode ! a, labelToNode ! b, (a, b))) .
      concatMap (\(a, b) -> map (a, ) b) . assocs $
      dic
    labelToNode = fromList . map (\(a, b) -> (b, a)) $ nodes

graphToViz :: Gr a (a, a) -> DotGraph Node
graphToViz = graphToDot nonClusteredParams
