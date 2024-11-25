{-# LANGUAGE TupleSections #-}

module Day12
  ( part1
  , part2
  ) where

import           Data.Graph.Inductive.Graph     (mkUGraph)
import           Data.Graph.Inductive.Query.DFS (components)
import           Data.Set                       (fromList, toList)
import           Helpers.Graph                  (Gr)
import           Helpers.Parsers                (numbers)

type System = Gr () ()

buildGraph :: [[Int]] -> System
buildGraph pipes = mkUGraph nodes (outEdges ++ inEdges)
  where
    outEdges = concatMap (\(x:xs) -> map (x, ) xs) pipes
    inEdges = concatMap (\(x:xs) -> map (, x) xs) pipes
    nodes = toList . fromList . concat $ pipes

part1 :: Bool -> String -> String
part1 _ =
  show . length . head . filter (elem 0) . components . buildGraph . numbers

part2 :: Bool -> String -> String
part2 _ = show . length . components . buildGraph . numbers
