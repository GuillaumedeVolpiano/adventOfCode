module Day16
  ( part1
  , part2
  ) where

import           Text.Regex.TDFA    (getAllTextMatches, (=~))

import           Data.Graph         (Edge, Graph, Vertex, edges, graphFromEdges,
                                     vertices)
import           Data.List          as L (map, null, tails)
import           Data.Map           as M (Map, alter, assocs, empty, map, (!))
import           Data.Maybe         (Maybe (Just, Nothing), fromJust)
import           Data.Sequence      as Sq (Seq ((:<|), (:|>)), null, singleton,
                                           (><))
import           Data.Set           as St (Set, disjoint, empty, insert,
                                           notMember, singleton)
import           Search             (NodeFromVertex, VertexFromKey,
                                     floydWarshall)

import           Debug.Trace

data State =
  State
    { theGraph         :: (Graph, NodeFromVertex String, VertexFromKey String)
    , shortestPaths    :: Map Vertex [(Vertex, Int)]
    , curPos           :: String
    , opened           :: Set Vertex
    , time             :: Int
    , releasedPressure :: Int
    }

parseLine :: String -> (Int, String, [String])
parseLine s = (read flow, valve, valves)
  where
    (valve:flow:valves) = getAllTextMatches (s =~ "[A-Z]{2}|[0-9]+") :: [String]

reorg :: Map Edge Int -> Map Vertex [(Vertex, Int)]
reorg =
  foldl (\a ((b, c), d) -> alter (listAdd (c, d)) b a) M.empty .
  filter (\((a, b), _) -> a /= b) . assocs
  where
    listAdd v Nothing  = Just [v]
    listAdd v (Just l) = Just (v : l)

pressure :: (Int, String, [String]) -> Int
pressure (a, _, _) = a

open :: State -> (Vertex, Int) -> State
open state (vertex, dist) =
  state
    { curPos = valve
    , time = newTime
    , releasedPressure = newReleasedPressure
    , opened = insert vertex $ opened state
    }
  where
    newTime = time state - dist - 1
    curReleasedPressure = releasedPressure state
    (_, nodeFromVertex, _) = theGraph state
    (pressure, valve, _) = nodeFromVertex vertex
    newReleasedPressure = curReleasedPressure + newTime * pressure

explore :: [State] -> Seq State -> [State]
explore done seq
  | Sq.null seq = done
  | otherwise = explore newDone toSee
  where
    (cur :<| next) = seq
    curScore = releasedPressure cur
    remTime = time cur
    (graph, nodeFromVertex, vertexFromKey) = theGraph cur
    valve = curPos cur
    Just vertex = vertexFromKey valve
    possDest =
      filter (\(a, b) -> notMember a (opened cur) && (b < remTime - 2)) $
      shortestPaths cur ! vertex
    toSee = foldl (\a b -> open cur b :<| a) next possDest
    newDone = cur : done

part1 :: Bool -> String -> String
part1 _ = show . result1

result1 :: String -> Int
result1 input = maximum . L.map releasedPressure $ explo
  where
    graph@(_, nodeFromVertex, vertexFromKey) =
      graphFromEdges . L.map parseLine . lines $ input
    shortPaths =
      M.map (filter (\(b, _) -> (pressure . nodeFromVertex) b /= 0)) .
      reorg . floydWarshall $
      graph
    initialState = State graph shortPaths "AA" St.empty 30 0
    explo = explore [] . Sq.singleton $ initialState

part2 :: Bool -> String -> String
part2 _ input = show . maximum . L.map releasedPressure $ exploElephant
  where
    graph@(_, nodeFromVertex, vertexFromKey) =
      graphFromEdges . L.map parseLine . lines $ input
    shortPaths =
      M.map (filter (\(b, _) -> (pressure . nodeFromVertex) b /= 0)) .
      reorg . floydWarshall $
      graph
    initialState = State graph shortPaths "AA" St.empty 26 0
    explo = explore [] . Sq.singleton $ initialState
    exploElephant =
      concatMap (\a -> explore [] . Sq.singleton $ a {curPos = "AA", time = 26}) .
      filter (\a -> releasedPressure a > div (result1 input) 2 + 1) $
      explo
