module Day20
  ( part1
  , part2
  ) where

import           Data.Char                      (isUpper)
import           Data.Either                    (fromRight)
import           Data.Graph.Inductive.NodeMap   (NodeMap, mkMapGraph, mkNode_)
import           Data.Graph.Inductive.Query.BFS (level)
import           Data.List                      (foldl', foldr)
import           Data.Set                       as S (Set, empty, insert,
                                                      singleton, toList, union)
import           Helpers.Graph                  (Gr, Pos, assocsToDigraph, east,
                                                 north, origin, south, west)
import           Helpers.Parsers                (Parser)
import           Text.Megaparsec                (between, choice, many,
                                                 manyTill, optional, parse,
                                                 satisfy, sepBy, takeWhile1P,
                                                 try, (<|>))
import           Text.Megaparsec.Char           (char, upperChar)

type Facility = (Set Pos, Set (Pos, Pos, Int))

data Path
  = Straight String
  | Branch [Path]
  | FullPath [Path]
  deriving (Show)

parsePath :: Parser Path
parsePath = do
  char '^'
  FullPath <$> manyTill buildPath (char '$')

buildPath :: Parser Path
buildPath = do
  parseStraight <|> parseBranch

parseStraight :: Parser Path
parseStraight = do
  Straight <$> takeWhile1P Nothing isUpper

parseBranch :: Parser Path
parseBranch = do
  Branch
    <$> between
          (char '(')
          (char ')')
          (sepBy (FullPath <$> many buildPath) (char '|'))

buildEdge :: (Pos, Facility) -> Char -> (Pos, Facility)
buildEdge (pos, (nodes, edges)) nextChar =
  ( newPos
  , ( insert newPos nodes
    , insert (pos, newPos, 1) . insert (newPos, pos, 1) $ edges))
  where
    newPos =
      pos
        + case nextChar of
            'E' -> east
            'N' -> north
            'W' -> west
            'S' -> south

buildManyEdges :: String -> Pos -> (Set Pos, Facility) -> (Set Pos, Facility)
buildManyEdges straight pos (lPos, facility) = (insert newPos lPos, newFacility)
  where
    (newPos, newFacility) = foldl' buildEdge (pos, facility) straight

buildBranch :: [Path] -> Pos -> (Set Pos, Facility) -> (Set Pos, Facility)
buildBranch branch pos (lPos, facility) =
  foldr (followBranch pos) (lPos, facility) branch

followBranch :: Pos -> Path -> (Set Pos, Facility) -> (Set Pos, Facility)
followBranch pos path (lPos, facility) = (newLPos `union` lPos, newFacility)
  where
    (newLPos, newFacility) = buildNodeMap (singleton pos, facility) path

buildNodeMap :: (Set Pos, Facility) -> Path -> (Set Pos, Facility)
buildNodeMap (lPos, facility) (Straight p) =
  foldr (buildManyEdges p) (empty, facility) lPos
buildNodeMap (lPos, facility) (FullPath p) =
  foldl' buildNodeMap (lPos, facility) p
buildNodeMap (lPos, facility) (Branch b) =
  foldr (buildBranch b) (empty, facility) lPos

buildGraph :: String -> (Gr Pos Int, NodeMap Pos)
buildGraph input = mkMapGraph (toList nodeSet) (toList edgeSet)
  where
    (nodeSet, edgeSet) =
      snd
        . buildNodeMap (singleton origin, (singleton origin, empty))
        . fromRight (Branch [])
        . parse parsePath ""
        $ input

findFurthest :: (Gr Pos Int, NodeMap Pos) -> Int
findFurthest (graph, nodeMap) = maximum . map snd . level originNode $ graph
  where
    originNode = fst . mkNode_ nodeMap $ origin

findFar :: (Gr Pos Int, NodeMap Pos) -> Int
findFar (graph, nodeMap) =
  length . filter (\(_, a) -> a >= 1000) . level originNode $ graph
  where
    originNode = fst . mkNode_ nodeMap $ origin

part1 :: Bool -> String -> String
part1 _ = show . findFurthest . buildGraph

part2 :: Bool -> String -> String
part2 _ = show . findFar . buildGraph
