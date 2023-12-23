module Day23
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed as A (UArray, bounds, elems, inRange,
                                          indices, (!))
import           Data.List          as L (null)
import           Data.Map           as M (Map, empty, fromList, insert, member,
                                          size, (!))
import           Data.Maybe         (Maybe (Just, Nothing), catMaybes, mapMaybe)
import           Data.Sequence      as Sq (Seq ((:<|), (:|>)), null, singleton)
import           Data.Set           as St (Set, insert, notMember, singleton,
                                           size)
import           Helpers.Graph      (Pos, dirs, east, north, south, west)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

type Hikes = UArray Pos Char

type State = (Pos, Set Pos)

type NodeMap = Map Pos [Node]

type Node = (Pos, Int)

slopes = fromList [('^', north), ('v', south), ('<', west), ('>', east)]

neighbours :: Hikes -> Bool -> State -> [State]
neighbours hikes hasSlopes (pos, path) = nextStates
  where
    possDest
      | hasSlopes && hikes A.! pos `elem` "^v<>" = [slopes M.! (hikes A.! pos)]
      | otherwise = dirs
    nextStates =
      filter (\(p, _) -> notMember p path && inRange b p && hikes A.! p /= '#') .
      map (\d -> (pos + d, St.insert (pos + d) path)) $
      possDest
    b = bounds hikes

findNodes :: Hikes -> Pos -> Pos -> Bool -> Pos -> [Node]
findNodes hikes startPos goalPos hasSlopes pos =
  mapMaybe (followPath hikes startPos goalPos hasSlopes) .
  neighbours hikes hasSlopes $
  (pos, St.singleton pos)

followPath :: Hikes -> Pos -> Pos -> Bool -> State -> Maybe Node
followPath hikes startPos goalPos hasSlopes state@(pos, path)
  | pos == startPos || pos == goalPos = Just (pos, St.size path - 1)
  | L.null neighbs = Nothing
  | length neighbs == 1 =
    followPath hikes startPos goalPos hasSlopes . head $ neighbs
  | otherwise = Just (pos, St.size path - 1)
  where
    neighbs = neighbours hikes hasSlopes state

findAllNodes :: Hikes -> Pos -> Pos -> Bool -> Seq Pos -> NodeMap -> NodeMap
findAllNodes hikes startPos goalPos hasSlopes toSee nodeMap
  | Sq.null toSee = nodeMap
findAllNodes hikes startPos goalPos hasSlopes (t :<| oSee) nodeMap =
  findAllNodes
    hikes
    startPos
    goalPos
    hasSlopes
    newToSee
    (M.insert t nodesDists nodeMap)
  where
    nodesDists = findNodes hikes startPos goalPos hasSlopes t
    newToSee =
      foldl (:|>) oSee . filter (\p -> not . M.member p $ nodeMap) . map fst $
      nodesDists

findPaths :: NodeMap -> State -> Pos -> [Maybe Int]
findPaths nodeMap (pos, path) goalPos
  | pos == goalPos = [Just 0]
  | L.null nextSteps = [Nothing]
  | otherwise =
    concatMap
      (\(p, d) ->
         map (fmap (d +)) $ findPaths nodeMap (p, St.insert p path) goalPos)
      nextSteps
  where
    nextSteps = filter (\node -> fst node `notMember` path) $ nodeMap M.! pos

part1 :: Bool -> String -> String
part1 _ input =
  show .
  maximum . catMaybes . findPaths nodeMap (startPos, St.singleton startPos) $
  goalPos
  where
    hikes = arrayFromString input
    (_, V2 _ dy) = bounds hikes
    posPos = indices hikes
    startPos =
      head . filter (\pos@(V2 _ y) -> y == 0 && hikes A.! pos == '.') $ posPos
    goalPos =
      head . filter (\pos@(V2 _ y) -> y == dy && hikes A.! pos == '.') $ posPos
    nodeMap =
      findAllNodes hikes startPos goalPos True (Sq.singleton startPos) M.empty

part2 :: Bool -> String -> String
part2 _ input =
  show .
  maximum . catMaybes . findPaths nodeMap (startPos, St.singleton startPos) $
  goalPos
  --show . explore hikes False goalPos $ startState
  where
    hikes = arrayFromString input
    (_, V2 _ dy) = bounds hikes
    posPos = indices hikes
    startPos =
      head . filter (\pos@(V2 _ y) -> y == 0 && hikes A.! pos == '.') $ posPos
    goalPos =
      head . filter (\pos@(V2 _ y) -> y == dy && hikes A.! pos == '.') $ posPos
    nodeMap =
      findAllNodes hikes startPos goalPos False (Sq.singleton startPos) M.empty
