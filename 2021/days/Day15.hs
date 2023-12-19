module Day15
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, bounds, inRange, (!))
import           Data.Maybe         (Maybe (Just, Nothing), mapMaybe)
import           Helpers.Parsers    (digitArrayFromString)
import           Helpers.Search     (dijkstraGoalVal)
import           Linear.V2          (V2 (..))
import           Linear.V4          (V4 (..))

type Pos = V2 Int

type Cave = UArray Pos Int

type PosTwo = V4 Int

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

dirs = [north, south, east, west]

startPosOne = V2 0 0

startPosTwo = V4 0 0 0 0

neighboursOne :: Cave -> Pos -> [(Pos, Int)]
neighboursOne cave pos =
  map (\x -> (x, cave ! x)) . filter (inRange b) . map (pos +) $ dirs
  where
    b = bounds cave

neighboursTwo :: Cave -> PosTwo -> [(PosTwo, Int)]
neighboursTwo cave pos@(V4 w x y z) =
  map (\next@(V4 a b c d) -> (next, mod (((cave ! V2 a b) + c + d) - 1) 9 + 1)) .
  mapMaybe (moveAround . (V2 w x +)) $
  dirs
  where
    cb@(_, V2 mx my) = bounds cave
    moveAround new@(V2 a b)
      | inRange cb new = Just $ V4 a b y z
      | a > mx && y < 4 = Just $ V4 0 b (y + 1) z
      | a > mx = Nothing
      | a < 0 && y > 0 = Just $ V4 mx b (y - 1) z
      | a < 0 = Nothing
      | b > mx && z < 4 = Just $ V4 a 0 y (z + 1)
      | b > mx = Nothing
      | b < 0 && z > 0 = Just $ V4 a my y (z - 1)
      | b < 0 = Nothing

lowestRiskOne :: Cave -> Int
lowestRiskOne cave = dijkstraGoalVal startPosOne 0 (neighboursOne cave) endPos
  where
    (_, endPos) = bounds cave

lowestRiskTwo :: Cave -> Int
lowestRiskTwo cave = dijkstraGoalVal startPosTwo 0 (neighboursTwo cave) endPos
  where
    (_, V2 mx my) = bounds cave
    endPos = V4 mx my 4 4

part1 :: Bool -> String -> String
part1 _ = show . lowestRiskOne . digitArrayFromString

part2 :: Bool -> String -> String
part2 _ = show . lowestRiskTwo . digitArrayFromString
