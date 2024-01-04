module Day22
  ( part1
  , part2
  ) where

import           Data.Maybe      (Maybe (Just, Nothing), mapMaybe)
import           Helpers.Parsers (alpha, complexParser, numsAsStrings)
import           Linear.V3       (V3 (..))

data Cuboid =
  Cuboid
    { lower  :: Pos
    , upper  :: Pos
    , active :: Bool
    }
  deriving (Show)

type Pos = V3 Int

lowThreshold = -50

highThreshold = 50

cuboidIntersection :: Cuboid -> Cuboid -> Maybe Cuboid
cuboidIntersection c1 c2
  | maxMinX <= minMaxX && maxMinY <= minMaxY && maxMinZ <= minMaxZ =
    Just $
    Cuboid (V3 maxMinX maxMinY maxMinZ) (V3 minMaxX minMaxY minMaxZ) activity
  | otherwise = Nothing
  where
    (V3 x0 y0 z0) = lower c1
    (V3 x1 y1 z1) = lower c2
    (V3 x2 y2 z2) = upper c1
    (V3 x3 y3 z3) = upper c2
    maxMinX = max x0 x1
    maxMinY = max y0 y1
    maxMinZ = max z0 z1
    minMaxX = min x2 x3
    minMaxY = min y2 y3
    minMaxZ = min z2 z3
    -- to adapt
    activity = not (active c2)

step :: Cuboid -> [Cuboid] -> [Cuboid]
step cuboid
  | active cuboid = (cuboid :) . mapMaybe (cuboidIntersection cuboid)
  | otherwise = mapMaybe (cuboidIntersection cuboid)

toCuboid :: [[String]] -> Cuboid
toCuboid [[x], pos] = Cuboid (V3 x0 y0 z0) (V3 x1 y1 z1) (x == "on")
  where
    [x0, x1, y0, y1, z0, z1] = map read pos

reboot :: [Cuboid] -> [Cuboid]
reboot = foldl (\cs c -> step c cs ++ cs) []

volume :: Cuboid -> Int
volume (Cuboid (V3 x0 y0 z0) (V3 x1 y1 z1) ac) =
  onoff * (x1 - x0 + 1) * (y1 - y0 + 1) * (z1 - z0 + 1)
  where
    onoff
      | ac = 1
      | otherwise = -1

ons :: [Cuboid] -> Int
ons = sum . map volume

threshold :: Cuboid -> Maybe Cuboid
threshold (Cuboid (V3 x0 y0 z0) (V3 x1 y1 z1) ac)
  | x0 > highThreshold ||
      y0 > highThreshold ||
      z0 > highThreshold ||
      x1 < lowThreshold || y1 < lowThreshold || z1 < lowThreshold = Nothing
  | otherwise =
    Just .
    Cuboid
      (V3 (max x0 lowThreshold) (max y0 lowThreshold) (max z0 lowThreshold))
      (V3 (min x1 highThreshold) (min y1 highThreshold) (min z1 highThreshold)) $
    ac

part1 :: Bool -> String -> String
part1 _ =
  show .
  ons .
  reboot .
  mapMaybe (threshold . toCuboid) .
  complexParser
    [" "]
    [ alpha
    , numsAsStrings
    , numsAsStrings
    , numsAsStrings
    , numsAsStrings
    , numsAsStrings
    , numsAsStrings
    ]

part2 :: Bool -> String -> String
part2 _ =
  show .
  ons .
  reboot .
  map toCuboid .
  complexParser
    [" "]
    [ alpha
    , numsAsStrings
    , numsAsStrings
    , numsAsStrings
    , numsAsStrings
    , numsAsStrings
    , numsAsStrings
    ]
