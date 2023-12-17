module Day17
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, bounds, inRange, (!))
import           Data.Hashable      (Hashable, hashWithSalt)
import           Data.Map           as M (Map, elems, filterWithKey, keys,
                                          lookup)
import           Helpers.Parsers    (digitArrayFromString)
import           Helpers.Search     (astarVal, dijkstraGoal)
import           Linear.V2          (V2 (..))

import           Data.Char          (intToDigit)
import           Data.List.Split    (chunksOf)
import           Data.Maybe         (fromJust, isNothing)

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

left :: Pos -> Pos
left (V2 x y) = V2 y (-x)

right :: Pos -> Pos
right (V2 x y) = V2 (-y) x

moves :: Blocks -> Crucible -> [(Crucible, Pos)]
moves blocks =
  map (\x -> (x, pos x)) .
  filter (\(Crucible np _ na) -> inRange (bounds blocks) np && na <= 3) .
  nextMoves

nextMoves :: Crucible -> [Crucible]
nextMoves (Crucible p d a) =
  Crucible (p + d) d (a + 1) :
  map (\x -> Crucible (p + x d) (x d) 1) [left, right]

heatLoss :: Blocks -> Pos -> Pos -> Int
heatLoss blocks _ p = blocks ! p

heuristic :: Pos -> Crucible -> Int
heuristic (V2 a b) crucible = abs (a - c) + abs (b - d)
  where
    V2 c d = pos crucible

ultraMoves :: Int -> Int -> Blocks -> Crucible -> [(Crucible, Int)]
ultraMoves minMoves maxMoves blocks c@(Crucible p d nm) =
  map (\x -> (x, blocks ! pos x)) .
  filter (\(Crucible np _ na) -> inRange (bounds blocks) np && na <= maxMoves) $
  next
  where
    next
      | nm < minMoves = [Crucible (p + d) d (nm + 1)]
      | otherwise = nextMoves c

reconstructPath :: Crucible -> Map Crucible Crucible -> [Pos]
reconstructPath c paths
  | isNothing . M.lookup c $ paths = []
  | otherwise = pos c : reconstructPath (fromJust . M.lookup c $ paths) paths

renderPath :: Blocks -> [Pos] -> String
renderPath blocks path =
  unlines . chunksOf (mx + 1) $
  [ if V2 x y `elem` path
    then '>'
    else intToDigit $ blocks ! V2 x y
  | y <- [0 .. my]
  , x <- [0 .. mx]
  ]
  where
    (_, V2 mx my) = bounds blocks

part1 :: Bool -> String -> String
part1 _ input =
  show $
  astarVal
    startPos
    (V2 0 0)
    ((==) endGoal . pos)
    (moves blocks)
    (heuristic endGoal)
    (heatLoss blocks)
  where
    blocks = digitArrayFromString input
    startPos = Crucible start east 0
    (start, endGoal) = bounds blocks

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
        (ultraMoves 4 10 blocks)
        (\c -> pos c == endGoal && acc c >= 4)
    goSouthYoungMan =
      dijkstraGoal
        startPosSouth
        0
        (ultraMoves 4 10 blocks)
        (\c -> pos c == endGoal && acc c >= 4)
    eastVal =
      minimum . elems . filterWithKey (\c _ -> pos c == endGoal && acc c >= 4) $
      fst goEastYoungMan
    southVal =
      minimum . elems . filterWithKey (\c _ -> pos c == endGoal && acc c >= 4) $
      fst goSouthYoungMan
    eastGoal =
      head . keys . filterWithKey (\c _ -> pos c == endGoal && acc c >= 4) $
      snd goEastYoungMan
    southGoal =
      head . keys . filterWithKey (\c _ -> pos c == endGoal && acc c >= 4) $
      snd goSouthYoungMan
    eastPath = renderPath blocks . reconstructPath eastGoal $ snd goEastYoungMan
    southPath =
      renderPath blocks . reconstructPath southGoal $ snd goSouthYoungMan
