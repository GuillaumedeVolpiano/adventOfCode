module Day17
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed         (UArray, bounds, inRange, (!))
import           Data.ByteString            (ByteString)
import           Data.Hashable              (Hashable, hashWithSalt)
import           Data.HashMap.Strict        as M (HashMap, elems, empty, insert,
                                                  lookup, singleton)
import           Data.HashPSQ               as Q (insert, singleton)
import           Data.Maybe                 (fromJust)
import           Helpers.Parsers.ByteString (digitArrayFromByteString)
import           Helpers.Search             (dijkstraMech)
import           Linear.V2                  (V2 (..))

import           Debug.Trace

data Crucible = Crucible
  { pos :: Pos
  , dir :: Pos
  , acc :: Int
  } deriving (Show, Eq, Ord)

type Blocks = UArray Pos Int

type Pos = V2 Int

instance Hashable Crucible where
  hashWithSalt salt (Crucible a b c) =
    salt `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c

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
  Crucible (p + d) d (a + 1)
    : map (\x -> Crucible (p + x d) (x d) 1) [left, right]

heatLoss :: Blocks -> Pos -> Pos -> Int
heatLoss blocks _ p = blocks ! p

moves :: Int -> Int -> Blocks -> Crucible -> [(Crucible, Int)]
moves minMoves maxMoves blocks c@(Crucible p d nm) =
  map (\x -> (x, blocks ! pos x))
    . filter
        (\(Crucible np _ na) -> inRange (bounds blocks) np && na <= maxMoves)
    $ next
  where
    next
      | nm < minMoves = [Crucible (p + d) d (nm + 1)]
      | otherwise = nextMoves c

part1 :: Bool -> ByteString -> String
part1 _ input = show dijkVal
  where
    blocks = digitArrayFromByteString input
    startPos = Crucible start east 0
    (start, endGoal) = bounds blocks
    (Just actualGoal, (dijVals, _)) =
      dijkstraMech
        (Q.singleton startPos 0 startPos)
        (M.singleton startPos 0)
        M.empty
        (moves minMove1 maxMove1 blocks)
        ((==) endGoal . pos)
    dijkVal = fromJust . M.lookup actualGoal $ dijVals

part2 :: Bool -> ByteString -> String
part2 _ input = show dijkVal
  where
    blocks = digitArrayFromByteString input
    startPosEast = Crucible start east 0
    startPosSouth = Crucible start south 0
    (start, endGoal) = bounds blocks
    (Just actualGoal, (dijVals, _)) =
      dijkstraMech
        (Q.insert startPosEast 0 startPosEast
           $ Q.singleton startPosSouth 0 startPosSouth)
        (M.insert startPosEast 0 $ M.singleton startPosSouth 0)
        M.empty
        (moves minMove2 maxMove2 blocks)
        (\c -> pos c == endGoal && acc c >= 4)
    dijkVal = fromJust . M.lookup actualGoal $ dijVals
