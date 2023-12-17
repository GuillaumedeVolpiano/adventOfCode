module Day17
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, bounds, inRange, (!))
import           Data.Hashable      (Hashable, hashWithSalt)
import           Data.Map           as M (Map, elems, filterWithKey)
import           Helpers.Parsers    (digitArrayFromString)
import           Helpers.Search     (astarVal, dijkstraGoal)
import           Linear.V2          (V2 (..))

data Crucible =
  Crucible
    { pos :: Pos
    , dir :: Pos
    , acc :: Int
    }
  deriving (Show, Eq, Ord)

type Blocks = UArray Pos Int

type Pos = V2 Int

instance Hashable Crucible where
  hashWithSalt salt (Crucible (V2 a b) (V2 c d) e) =
    salt + a * 10 ^ 16 + b * 10 ^ 12 + c * 10 ^ 8 + d * 10 ^ 4 + e

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

minMove1 = 0

maxMove1 = 3

minMove2 = 4

maxMove2 = 10

left :: Pos -> Pos
left (V2 x y) = V2 y (-x)

right :: Pos -> Pos
right (V2 x y) = V2 (-y) x

nextMoves :: Crucible -> [Crucible]
nextMoves (Crucible p d a) =
  Crucible (p + d) d (a + 1) :
  map (\x -> Crucible (p + x d) (x d) 1) [left, right]

heatLoss :: Blocks -> Pos -> Pos -> Int
heatLoss blocks _ p = blocks ! p

moves :: Int -> Int -> Blocks -> Crucible -> [(Crucible, Int)]
moves minMoves maxMoves blocks c@(Crucible p d nm) =
  map (\x -> (x, blocks ! pos x)) .
  filter (\(Crucible np _ na) -> inRange (bounds blocks) np && na <= maxMoves) $
  next
  where
    next
      | nm < minMoves = [Crucible (p + d) d (nm + 1)]
      | otherwise = nextMoves c

part1 :: Bool -> String -> String
part1 _ input = show dijkVal
  where
    blocks = digitArrayFromString input
    startPos = Crucible start east 0
    (start, endGoal) = bounds blocks
    dijked =
      dijkstraGoal
        startPos
        0
        (moves minMove1 maxMove1 blocks)
        ((==) endGoal . pos)
    dijkVal =
      minimum . elems . filterWithKey (\c _ -> pos c == endGoal) . fst $ dijked

part2 :: Bool -> String -> String
part2 _ input = show $ min eastVal southVal
  where
    blocks = digitArrayFromString input
    startPosEast = Crucible start east 0
    startPosSouth = Crucible start south 0
    (start, endGoal) = bounds blocks
    goEastYoungMan =
      dijkstraGoal
        startPosEast
        0
        (moves minMove2 maxMove2 blocks)
        (\c -> pos c == endGoal && acc c >= minMove2)
    goSouthYoungMan =
      dijkstraGoal
        startPosSouth
        0
        (moves minMove2 maxMove2 blocks)
        (\c -> pos c == endGoal && acc c >= minMove2)
    eastVal =
      minimum .
      elems . filterWithKey (\c _ -> pos c == endGoal && acc c >= minMove2) $
      fst goEastYoungMan
    southVal =
      minimum .
      elems . filterWithKey (\c _ -> pos c == endGoal && acc c >= minMove2) $
      fst goSouthYoungMan
