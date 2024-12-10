{-# LANGUAGE TupleSections #-}

module Day10
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed             (UArray, amap, bounds, inRange,
                                                 range, (!))
import           Data.Char                      (digitToInt, isDigit)
import           Data.Graph.Inductive.Graph     (Node, lab, labNodes, suc)
import           Data.Graph.Inductive.Query.BFS (bfs)
import           Data.Text                      (Text)
import           Helpers.Graph                  (Gr, Pos, assocsToGraph, east,
                                                 north, south, west)
import           Helpers.Parsers.Text           (arrayFromText)

import           Debug.Trace

type TopoMap = Gr (Pos, Int) ()

dirs = [north, south, east, west]

buildTopoMap :: UArray Pos Int -> TopoMap
buildTopoMap arrayMap =
  assocsToGraph . map neighbours . range . bounds $ arrayMap
  where
    neighbours p =
      ((p, arrayMap ! p), )
        . map (, ())
        . filter ((== (arrayMap ! p) + 1) . snd)
        . map (\n -> (n, arrayMap ! n))
        . filter (inRange . bounds $ arrayMap)
        . map (p +)
        $ dirs

findTrails :: TopoMap -> Int
findTrails topoMap =
  sum
    . map
        (length
           . filter ((== Just 9) . fmap snd . lab topoMap)
           . flip bfs topoMap
           . fst)
    . filter ((== 0) . snd . snd)
    . labNodes
    $ topoMap

findAllTrails :: TopoMap -> Node
findAllTrails topoMap = length . foldr (explore topoMap) trailHeads $ [1 .. 9]
  where
    trailHeads = map fst . filter ((== 0) . snd . snd) . labNodes $ topoMap

explore :: TopoMap -> Int -> [Node] -> [Node]
explore topoMap _ = concatMap (suc topoMap)

testDigit :: Char -> Int
testDigit '.' = -1
testDigit c   = digitToInt c

part1 :: Bool -> Text -> String
part1 _ = show . findTrails . buildTopoMap . amap testDigit . arrayFromText

part2 :: Bool -> Text -> String
part2 _ = show . findAllTrails . buildTopoMap . amap testDigit . arrayFromText
