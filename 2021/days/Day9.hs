module Day9
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed as A (UArray, bounds, inRange, range, (!))
import           Data.List          (sortBy)
import           Data.Set           (Set, empty, fromList, insert)
import           Helpers.Parsers    (digitArrayFromString)
import           Helpers.Search     (floodFill, treeSize)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

type HeightMap = UArray Pos Int

up = V2 0 (-1)

down = V2 0 1

right = V2 1 0

left = V2 (-1) 0

fourDirs = [up, down, left, right]

neighbours :: HeightMap -> Pos -> [Pos]
neighbours heightMap pos =
  filter (bounds heightMap `inRange`) . map (pos +) $ fourDirs

isLowPoint :: HeightMap -> Pos -> Bool
isLowPoint heightMap pos =
  (heightMap ! pos <) . minimum . map (heightMap !) . neighbours heightMap $ pos

lowPoints :: HeightMap -> [Pos]
lowPoints heightMap = filter (isLowPoint heightMap) . range . bounds $ heightMap

scoreLowPoints :: HeightMap -> Int
scoreLowPoints heightMap =
  sum . map ((1 +) . (!) heightMap) . lowPoints $ heightMap

basins :: HeightMap -> [Int]
basins heightMap = sortBy (flip compare) . map treeSize . floodFill edges $ pos
  where
    pos = lowPoints heightMap
    edges =
      map
        (\x ->
           ( heightMap ! x
           , x
           , filter ((< 9) . (!) heightMap) . neighbours heightMap $ x)) .
      filter ((< 9) . (!) heightMap) . range . bounds $
      heightMap

part1 :: Bool -> String -> String
part1 _ = show . scoreLowPoints . digitArrayFromString

part2 :: Bool -> String -> String
part2 _ = show . product . take 3 . basins . digitArrayFromString
