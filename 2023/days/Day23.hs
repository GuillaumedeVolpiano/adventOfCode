module Day23
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed as A (UArray, bounds, elems, inRange,
                                          indices, (!))
import           Data.List          as L (null)
import           Data.Map           as M (Map, empty, fromList, insert, member,
                                          size, (!))
import           Data.Maybe         (Maybe (Just, Nothing), mapMaybe)
import           Data.Sequence      as Sq (Seq ((:<|), (:|>)), null, singleton)
import           Data.Set           as St (Set, insert, notMember, singleton,
                                           size)
import           Helpers.Graph      (Pos, dirs, east, north, south, west)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

type Hikes = UArray Pos Char

type State = (Pos, Set Pos)

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

explore :: Hikes -> Bool -> Pos -> State -> Int
explore hikes hasSlopes goal state@(pos, path)
  | pos == goal = St.size path - 1
  | L.null nextPos = negate 1
  | otherwise = maximum . map (explore hikes hasSlopes goal) $ nextPos
  where
    nextPos = neighbours hikes hasSlopes state

findNodes :: Hikes -> Pos -> Pos -> Bool -> Pos -> [(Pos, Int)]
findNodes hikes startPos goalPos hasSlopes pos =
  mapMaybe (followPath hikes startPos goalPos hasSlopes) .
  neighbours hikes hasSlopes $
  (pos, St.singleton pos)

followPath :: Hikes -> Pos -> Pos -> Bool -> State -> Maybe (Pos, Int)
followPath hikes startPos goalPos hasSlopes state@(pos, path)
  | pos == startPos || pos == goalPos = Just (pos, St.size path - 1)
  | L.null neighbs = Nothing
  | length neighbs == 1 =
    followPath hikes startPos goalPos hasSlopes . head $ neighbs
  | otherwise = Just (pos, St.size path - 1)
  where
    neighbs = neighbours hikes hasSlopes state

findAllNodes ::
     Hikes
  -> Pos
  -> Pos
  -> Bool
  -> Seq Pos
  -> Map Pos [(Pos, Int)]
  -> Map Pos [(Pos, Int)]
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

part1 :: Bool -> String -> String
part1 _ input = show . explore hikes True goalPos $ startState
  where
    hikes = arrayFromString input
    (_, V2 _ dy) = bounds hikes
    posPos = indices hikes
    startPos =
      head . filter (\pos@(V2 _ y) -> y == 0 && hikes A.! pos == '.') $ posPos
    goalPos =
      head . filter (\pos@(V2 _ y) -> y == dy && hikes A.! pos == '.') $ posPos
    isGoal pos = fst pos == goalPos
    allSpots = length . filter (/= '#') . elems $ hikes
    startState = (startPos, St.singleton startPos)

part2 :: Bool -> String -> String
part2 _ input =
  show .
  M.size . findAllNodes hikes startPos goalPos True (Sq.singleton startPos) $
  M.empty
  --show . explore hikes False goalPos $ startState
  where
    hikes = arrayFromString input
    (_, V2 _ dy) = bounds hikes
    posPos = indices hikes
    startPos =
      head . filter (\pos@(V2 _ y) -> y == 0 && hikes A.! pos == '.') $ posPos
    goalPos =
      head . filter (\pos@(V2 _ y) -> y == dy && hikes A.! pos == '.') $ posPos
    isGoal pos = fst pos == goalPos
    allSpots = length . filter (/= '#') . elems $ hikes
    startState = (startPos, St.singleton startPos)
