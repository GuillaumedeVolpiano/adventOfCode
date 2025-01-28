module Day17
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed         (UArray, bounds, inRange, (!))
import           Data.Bifunctor             (first)
import           Data.Bits                  (shiftL, shiftR, (.&.))
import           Data.ByteString            (ByteString)
import qualified Data.IntMap.Strict         as M (IntMap, elems, empty, insert,
                                                  lookup, singleton)
import qualified Data.IntPSQ                as Q (insert, singleton)
import           Data.List                  (iterate', tails)
import           Data.Maybe                 (fromJust)
import           Data.Monoid                (Sum (Sum))
import           Helpers.Parsers.ByteString (digitArrayFromByteString)
import           Helpers.Search.Int         (IntLike, dijkstraMech, fromInt,
                                             monadDijkstra, toInt)
import           Linear.V2                  (V2 (..))
import           Linear.Vector              ((*^))

import           Debug.Trace

data Crucible = Crucible
  { pos :: {-# UNPACK #-} !Pos
  , dir :: {-# UNPACK #-} !Pos
  , acc :: {-# UNPACK #-} !Int
  } deriving (Show, Eq, Ord)

type Blocks = UArray Pos HeatLoss

type HeatLoss = Int

type Pos = V2 Int

instance Integral a => IntLike (V2 a) where
  toInt (V2 a b) = fromIntegral a + shiftL (fromIntegral b) 8
  fromInt i = V2 (fromIntegral $ i .&. 255) (fromIntegral $ shiftR i 8)

instance IntLike Crucible where
  toInt (Crucible pos dir acc) = toInt pos + shiftL intPos 16 + shiftL acc 18
    where
      intPos
        | dir == north = 0
        | dir == east = 1
        | dir == south = 2
        | dir == west = 3
  fromInt i = Crucible pos dir acc
    where
      pos = fromInt $ i .&. 65535
      dir = posInt $ shiftR i 16 .&. 3
      acc = shiftR i 18
      posInt 0 = north
      posInt 1 = east
      posInt 2 = south
      posInt 3 = west

north = V2 0 (-1)

south = V2 0 1

east = V2 1 0

west = V2 (-1) 0

minMove1 = 1 -- (as we integrate the turn in the sum of the moves)

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

moves :: Int -> Int -> Blocks -> Int -> [(Int, Int)]
moves minMoves maxMoves blocks intc =
  map (\x -> (toInt x, blocks ! pos x))
    . filter
        (\(Crucible np _ na) -> inRange (bounds blocks) np && na <= maxMoves)
    $ next
  where
    c@(Crucible p d nm) = fromInt intc
    next
      | nm < minMoves = [Crucible (p + d) d (nm + 1)]
      | otherwise = nextMoves c

-- Let's see if we can speed things up by returning allviable moves in one go.
-- If this work we can then take the acc parameter off Crucible ,saving some
-- memory space.
moves' :: Int -> Int -> Blocks -> Int -> [(Int, Int)]
moves' minMoves maxMoves blocks intc
  | length forward < minMoves = []
  | otherwise = concatMap turns forward
  where
    c@(Crucible p d nm) = fromInt intc
    forward =
      map (foldr totalHeat (c, 0))
        . filter ((>= minMoves) . length)
        . tails
        . filter (inRange (bounds blocks) . pos)
        . map (\x -> Crucible (p + (x *^ d)) d nm)
        $ [maxMoves,maxMoves - 1 .. 1]
    totalHeat c@(Crucible p _ _) (_, heat) = (c, heat + (blocks ! p))
    turns (Crucible p d nm, heat) =
      map (\x -> (toInt $ Crucible p (x d) nm, heat)) [left, right]

neighbours :: Crucible -> Blocks -> [(Crucible, Sum HeatLoss)]
neighbours crucible blocks =
  trace
    (show crucible ++ "\n " ++ show ns)
    concatMap
    (filter (inRange (bounds blocks) . pos . fst) . turn)
    . take 3
    . drop 1
    . iterate' accuMove
    $ (crucible, 0)
  where
    ns =
      concatMap (filter (inRange (bounds blocks) . pos . fst) . turn)
        . take 3
        . drop 1
        . iterate' accuMove
        $ (crucible, 0)
    accuMove (c, acc) = (c', acc + blocks ! pos c')
      where
        c' = c {pos = pos c + dir c}
    turn (c, acc) =
      [ (c {dir = left . dir $ c}, Sum acc)
      , (c {dir = right . dir $ c}, Sum acc)
      ]

part1 :: Bool -> ByteString -> String
part1 _ input = show dijkVal
  where
    blocks = digitArrayFromByteString input
    startPos = Crucible start east 0
    (start, endGoal) = bounds blocks
    (Just actualGoal, (dijVals, _)) =
      dijkstraMech
        (Q.insert (toInt startPos) 0 ()
           $ Q.singleton (toInt $ Crucible start south 0) 0 ())
        (M.insert (toInt startPos) 0
           $ M.singleton (toInt $ Crucible start south 0) 0)
        M.empty
        (moves' minMove1 maxMove1 blocks)
        (\c -> pos (fromInt c) == endGoal)
    dijkVal = fromJust . M.lookup actualGoal $ dijVals

part2 :: Bool -> ByteString -> String
part2 _ input = show dijkVal
  where
    blocks = digitArrayFromByteString input
    startPosEast = toInt $ Crucible start east 0
    startPosSouth = toInt $ Crucible start south 0
    (start, endGoal) = bounds blocks
    (Just actualGoal, (dijVals, _)) =
      dijkstraMech
        (Q.insert startPosEast 0 () $ Q.singleton startPosSouth 0 ())
        (M.insert startPosEast 0 $ M.singleton startPosSouth 0)
        M.empty
        (moves minMove2 maxMove2 blocks)
        (\c -> pos (fromInt c) == endGoal && acc (fromInt c) >= 4)
    dijkVal = fromJust . M.lookup actualGoal $ dijVals
