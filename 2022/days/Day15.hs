module Day15 (part1, part2) where
import           Data.List.Split    (splitOn)

import           Data.List          as L (filter, nub, sortBy)
import           Data.Map           as M (Map, elems, filter, fromList,
                                          mapWithKey)
import           Data.Maybe         (Maybe (Just, Nothing), catMaybes)
import           Debug.Trace
import           Linear.V2          (V2 (..))

day = 15

type Pos = V2 Int

-- Mapping sensors to beacons
type Plan = Map Pos Pos

compareAbs :: Pos -> Pos -> Ordering
compareAbs (V2 a _) (V2 b _) = compare a b

dist :: Pos -> Pos -> Int
dist (V2 a b) (V2 c d) = abs (a - c) + abs (b - d)

linePos :: Int -> Pos -> Pos -> Maybe (Pos, Pos)
linePos row sensor@(V2 x y) beacon
  | halfWidth < 0 = Nothing
  | otherwise = Just (V2 from row, V2 to row)
  where
    d = dist sensor beacon
    halfWidth = d - abs (row - y)
    from = x - halfWidth
    to = x + halfWidth

noSensorOnRow :: Int -> Plan -> [(Pos, Pos)]
noSensorOnRow row = catMaybes . elems . mapWithKey (linePos row)

merge :: [(Pos, Pos)] -> [(Pos, Pos)]
merge [a] = [a]
merge ((fi, fe):(si, se):xs)
  | compareAbs fe se /= LT = merge ((fi, fe) : xs)
  | compareAbs fe si /= LT = merge ((fi, se) : xs)
  | si - fe == V2 1 0 = merge ((fi, se) : xs)
  | otherwise = (fi, fe) : merge ((si, se) : xs)

rowSize :: [(Pos, Pos)] -> Int
rowSize = sum . map (\(a, b) -> dist a b + 1)

parseLine :: String -> (Pos, Pos)
parseLine s = (V2 x y, V2 a b)
  where
    w = words s
    x = read . init . drop 2 $ w !! 2
    y = read . init . drop 2 $ w !! 3
    a = read . init . drop 2 $ w !! 8
    b = read . drop 2 $ w !! 9

sensorsOnRow :: Int -> Plan -> Int
sensorsOnRow row = length . nub . elems . M.filter (\(V2 _ y) -> y == row)

sensorSort :: (Pos, Pos) -> (Pos, Pos) -> Ordering
sensorSort (a, _) (b, _) = compareAbs a b

inRange :: Int -> (Pos, Pos) -> Bool
inRange maxRow (V2 a _, V2 b _) = b >= 0 && a <= maxRow

tuningFreq :: [(Pos, Pos)] -> Int
tuningFreq ((_, V2 x y):_) = 4000000 * (x + 1) + y

row :: Bool -> Int
row test
  | test = 10
  | otherwise = 2000000

maxRow :: Bool -> Int
maxRow test
  | test = 20
  | otherwise = 4000000

plan = fromList . map parseLine . lines 

filledRows mr p = 
        map
          (\x ->
             merge . sortBy sensorSort . L.filter (inRange mr) $
             noSensorOnRow x p)
          [0 .. mr]

part1 :: Bool -> String -> String
part1 test input = show $
    (rowSize . merge . sortBy sensorSort . noSensorOnRow r $ p) -
    sensorsOnRow r p
      where
        r = row test
        p = plan input

part2 :: Bool -> String ->String
part2 test input = show . head . map tuningFreq . L.filter (\x -> length x > 1) . filledRows mr $ p
  where
    mr = maxRow test
    p = plan input
