module Day6
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed         (UArray, assocs, (!?))
import           Data.Bits                  (shiftL, shiftR, (.&.))
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as B (length)
import           Data.ByteString.Char8      (index)
import           Data.IntSet                (IntSet, empty, insert, member,
                                             size)
import qualified Data.IntSet                as S (filter)
import           Data.List                  as L (filter, unfoldr)
import           Data.Maybe                 (fromJust, isNothing)
import           Data.Vector.Unboxed        (Vector, elemIndex, generate, (!))
import qualified Helpers.Parsers.ByteString as P (lines)

-- first 8 bits for x, next 8 bits for y, then two bits for direction,
-- starting at 0 for north, rotating clockwise
type Guard = Int

type Map = Vector Int

type Seen = IntSet

type Obstacle = Int

getGuard :: Map -> Guard
getGuard = fromJust . elemIndex 2

right :: Guard -> Guard
right guard
  | shiftR guard 16 == 3 = guard .&. 65535
  | otherwise = guard + 2 ^ 16

left :: Guard -> Guard
left guard
  | shiftR guard 16 == 0 = guard + shiftL 3 16
  | otherwise = guard - 2 ^ 16

move :: Guard -> Guard
move guard
  | dir == 0 = guard - 2 ^ 8
  | dir == 1 = guard + 1
  | dir == 2 = guard + 2 ^ 8
  | dir == 3 = guard - 1
  where
    dir = shiftR guard 16

followGuard :: Map -> (Seen, Guard) -> Maybe (Seen, (Seen, Guard))
followGuard map (seen, guard)
  | map ! pos == 3 = Nothing
  | map ! pos == 0 = Just (seen, (seen, backtrack guard))
  | otherwise = Just (seen', (seen', guard'))
  where
    guard' = move guard
    seen' = insert pos seen
    pos = guard .&. 65535

backtrack :: Guard -> Guard
backtrack = left . move . right . right

track :: Map -> Seen
track map = last . unfoldr (followGuard map) $ (empty, guard)
  where
    guard = getGuard map

isLoop :: Map -> Guard -> Seen -> Obstacle -> Bool
isLoop map guard seen obstacle
  | guard `member` seen = True
  | map ! pos == 3 = False
  | pos == obstacle || map ! pos == 0 =
    isLoop map (backtrack guard) seen' obstacle
  | otherwise = isLoop map guard' seen obstacle
  where
    guard' = move guard
    seen' = insert guard seen
    pos = guard .&. 65535

findLoops :: Map -> Int
findLoops map = size . S.filter (isLoop map guard empty) . track $ map
  where
    guard = getGuard map

createMap :: ByteString -> Map
createMap input = generate (2 ^ 16) indexMap
  where
    linedInput = P.lines input
    yMax = length linedInput
    xMax = B.length . head $ linedInput
    indexMap i
      | x >= xMax || y >= yMax = 3
      | otherwise = classify $ linedInput !! y `index` x
      where
        x = i .&. 255
        y = shiftR i 8
    classify '^' = 2
    classify '#' = 0
    classify _   = 1

part1 :: Bool -> ByteString -> String
part1 _ = show . size . track . createMap

part2 :: Bool -> ByteString -> String
part2 _ = show . findLoops . createMap
