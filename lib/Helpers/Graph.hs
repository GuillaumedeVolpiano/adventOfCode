{-# LANGUAGE TupleSections #-}

module Helpers.Graph
  ( dicToGraph
  , graphToViz
  ) where

import           Data.Graph.Inductive.Graph        (Graph, Node, mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     (DotGraph, Labellable,
                                                    graphToDot, quickParams, printDotGraph)
import           Data.Map                          (Map, assocs, fromList, keys,
                                                    (!))
import Data.Text.Lazy (Text)

dicToGraph :: Map String [String] -> Gr String String
dicToGraph dic = mkGraph nodes edges
  where
    nodes = zip [0 ..] . keys $ dic
    edges =
      map (\(a, b) -> (labelToNode ! a, labelToNode ! b, a ++ " " ++ b)) .
      concatMap (\(a, b) -> map (a, ) b) . assocs $
      dic
    labelToNode = fromList . map (\(a, b) -> (b, a)) $ nodes

graphToViz :: Gr String String -> Text
graphToViz = printDotGraph . graphToDot quickParams
