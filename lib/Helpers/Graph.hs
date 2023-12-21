{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}

module Helpers.Graph
  ( dicToGraph
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
import           Data.Graph.Inductive.Graph        (Graph, Node, mkGraph)
import           Data.Graph.Inductive.PatriciaTree (Gr)
import           Data.GraphViz                     (DotGraph, Labellable,
                                                    graphToDot, printDotGraph,
                                                    quickParams)
import           Data.Map                          (Map, assocs, fromList, keys,
                                                    (!))
import           Data.Text.Lazy                    (Text)
import           Linear.V2                         (V2 (..))

type Pos = V2 Int

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

dirs = [north, south, east, west]

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

neighbours :: (IArray UArray a) => UArray Pos a -> Pos -> [Pos]
neighbours a p = filter (inRange b) . map (p +) $ dirs
  where
    b = bounds a
