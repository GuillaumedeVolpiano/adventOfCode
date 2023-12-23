module Day23
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed as A (UArray, bounds, elems, inRange,
                                          indices, (!))
import           Data.Map           as M (fromList, (!))
import           Data.Set           (Set, insert, notMember, singleton, size)
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
      map (\d -> (pos + d, insert (pos + d) path)) $
      possDest
    b = bounds hikes

explore :: Hikes -> Bool -> Pos -> State -> Int
explore hikes hasSlopes goal state@(pos, path)
  | pos == goal = size path - 1
  | null nextPos = negate 1
  | otherwise = maximum . map (explore hikes hasSlopes goal) $ nextPos
  where
    nextPos = neighbours hikes hasSlopes state

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
    startState = (startPos, singleton startPos)

part2 :: Bool -> String -> String
part2 _ input = show . explore hikes False goalPos $ startState
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
    startState = (startPos, singleton startPos)
