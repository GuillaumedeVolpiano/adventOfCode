module Day19
  ( part1
  , part2
  ) where

import           Data.Bifunctor  (first, second)
import           Data.List       as L (filter, groupBy, map, partition, sortBy)
import           Data.List.Split (splitWhen)
import           Data.Set        as St (Set, empty, filter, findMax, fromList,
                                        insert, intersection, map, size, toList,
                                        union, unions)
import           Helpers.Parsers (integers)
import           Linear.V3       (V3 (..))

import           Debug.Trace

type Beacon = V3 Int

type Scanner = Int

type Reading = (Scanner, Set Beacon)

type ReadingDistances = (Scanner, Set (Beacon, Set Int), Set Beacon)

type ScanDistances = Set Int

positions :: [Beacon -> Beacon]
positions = concatMap (\f -> L.map (f .) rotations) facing
  where
    facing =
      [ \(V3 x y z) -> V3 x y z
      , \(V3 x y z) -> V3 (-x) y (-z)
      , \(V3 x y z) -> V3 x (-z) y
      , \(V3 x y z) -> V3 x z (-y)
      , \(V3 x y z) -> V3 (-z) y x
      , \(V3 x y z) -> V3 z y (-x)
      ]
    rotations =
      [ \(V3 x y z) -> V3 x y z
      , \(V3 x y z) -> V3 (-y) x z
      , \(V3 x y z) -> V3 (-x) (-y) z
      , \(V3 x y z) -> V3 y (-x) z
      ]

distance :: Beacon -> Beacon -> Int
distance a b = dx ^ 2 + dy ^ 2 + dz ^ 2
  where
    (V3 dx dy dz) = a - b

manDist :: Beacon -> Beacon -> Int
manDist a b = abs dx + abs dy + abs dz
  where
    (V3 dx dy dz) = a - b

allDistances :: Reading -> ReadingDistances
allDistances (scanner, beacons) =
  ( scanner
  , St.map (\b -> (b, St.map (distance b) . St.filter (/= b) $ beacons)) beacons
  , empty)

allManDistances :: Set Beacon -> Set Int
allManDistances scanners =
  unions . St.map (\b -> St.map (manDist b) scanners) $ scanners

toScanDistances :: ReadingDistances -> ScanDistances
toScanDistances (_, beacread, _) = unions . St.map snd $ beacread

overlap ::
     [ReadingDistances]
  -> (ReadingDistances, ([ReadingDistances], [ReadingDistances]))
overlap (a:b) =
  ( a
  , partition
      (\c -> size (intersection (toScanDistances a) (toScanDistances c)) >= 66)
      b)

findOverlapBeacons ::
     ReadingDistances -> ReadingDistances -> Set (Beacon, Beacon)
findOverlapBeacons (a, b, _) (c, d, _) =
  St.map (second (head . toList)) .
  St.filter (\(_, s) -> size s >= 1) .
  St.map
    (\(b1, d1) ->
       ( b1
       , St.map fst . St.filter (\(b2, d2) -> size (intersection d1 d2) >= 11) $
         d)) $
  b

absorbOverlaps ::
     (ReadingDistances, ([ReadingDistances], [ReadingDistances]))
  -> (ReadingDistances, [ReadingDistances])
absorbOverlaps (a, (b, c))
  | null b =
    error ("no overlaps " ++ show a ++ "\n" ++ show b ++ "\n" ++ show c)
  | otherwise = (absorbed, c)
  where
    absorbed = foldl transform a b

findTransformation :: Set (Beacon, Beacon) -> (Beacon -> Beacon)
findTransformation s = translation . transformation
  where
    transform t = St.map (\(a, b) -> t b - a) s
    transformation = head . L.filter ((== 1) . size . transform) $ positions
    difference = head . toList . transform $ transformation
    translation x = x - difference

transform :: ReadingDistances -> ReadingDistances -> ReadingDistances
transform a@(scanner, beacons, scanners) b@(_, otherBeacons, _) =
  (scanner, newBeacons, insert local scanners)
  where
    overlap = findOverlapBeacons a b
    transformation = findTransformation overlap
    transformed = St.map (first transformation) otherBeacons
    newBeacons =
      fromList .
      L.map (foldl (\(_, b) (c, d) -> (c, b `union` d)) (V3 0 0 0, empty)) .
      groupBy (\a b -> fst a == fst b) .
      sortBy (\a b -> compare (fst a) (fst b)) . toList $
      beacons `union` transformed
    local = transformation (V3 0 0 0)

consume ::
     (ReadingDistances, ([ReadingDistances], [ReadingDistances]))
  -> ReadingDistances
consume toConsume
  | null remaining = consumed
  | otherwise = consume . overlap $ (consumed : remaining)
  where
    (consumed, remaining) = absorbOverlaps toConsume

part1 :: Bool -> String -> String
part1 _ =
  show .
  size .
  (\(_, a, _) -> a) .
  consume .
  overlap .
  L.map
    (allDistances .
     (\((a:_):b) -> (a, fromList . L.map (\[c, d, e] -> V3 c d e) $ b))) .
  splitWhen null . integers

part2 :: Bool -> String -> String
part2 _ =
  show .
  findMax .
  allManDistances .
  (\(_, _, a) -> a) .
  consume .
  overlap .
  L.map
    (allDistances .
     (\((a:_):b) -> (a, fromList . L.map (\[c, d, e] -> V3 c d e) $ b))) .
  splitWhen null . integers
