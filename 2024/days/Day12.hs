module Day12
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed         (UArray, assocs, bounds, range, (!))
import           Data.ByteString            (ByteString)
import           Data.List                  (groupBy, sort, sortBy)
import           Data.List                  as L (null)
import           Data.Maybe                 (mapMaybe)
import           Data.Set                   as S (Set, delete, deleteFindMin,
                                                  difference, fromList, insert,
                                                  member, null, singleton, size,
                                                  toList)
import           Data.Word                  (Word8)
import           Helpers.Graph              (Pos, dirs, east, neighbours, north,
                                             south, west)
import           Helpers.Parsers.ByteString (arrayFromByteString)
import           Linear.V2                  (V2 (..))

type Garden = [Plot]

type Plot = Set Pos

hGroup :: Pos -> [[Pos]] -> [[Pos]]
hGroup p [] = [[p]]
hGroup p@(V2 x0 y0) ls@(xs@((V2 x1 y1):_):ys)
  | y0 == y1 && abs (x0 - x1) == 1 = (p : xs) : ys
  | otherwise = [p] : ls

hSort :: Pos -> Pos -> Ordering
hSort (V2 x1 y1) (V2 x2 y2) = compare y1 y2 `mappend` compare x1 x2

vGroup :: Pos -> [[Pos]] -> [[Pos]]
vGroup p [] = [[p]]
vGroup p@(V2 x0 y0) ls@(xs@((V2 x1 y1):_):ys)
  | x0 == x1 && abs (y0 - y1) == 1 = (p : xs) : ys
  | otherwise = [p] : ls

buildGarden :: UArray Pos Word8 -> Garden
buildGarden array = garden plots
  where
    plots = fromList . range . bounds $ array
    garden poss
      | S.null poss = []
      | otherwise = region : garden poss'
      where
        (plot, p) = deleteFindMin poss
        (region, poss') = buildRegion array (singleton plot) [plot] p

buildRegion :: UArray Pos Word8 -> Plot -> [Pos] -> Set Pos -> (Plot, Set Pos)
buildRegion array seen toSee poss
  | L.null toSee = (seen, poss)
  | otherwise = buildRegion array seen' toSee' poss'
  where
    (pos:xs) = toSee
    crop = array ! pos
    toConsider =
      filter (\p -> (array ! p == crop) && not (p `member` seen))
        . neighbours array
        $ pos
    seen' = foldr insert seen toConsider
    toSee' = toConsider ++ xs
    poss' = foldr delete poss toConsider

getAreaPerim :: Plot -> (Int, Int)
getAreaPerim plot = (area, perim)
  where
    area = size plot
    perim = length . foldr outOfPlot [] $ plot
    outOfPlot pos p =
      (filter (not . flip member plot) . map (pos +) $ dirs) ++ p

getAreaSides :: Plot -> (Int, Int)
getAreaSides plot = (area, sides)
  where
    area = size plot
    sides = upSides + downSides + rightSides + leftSides
    upSides =
      sum
        . map
            (length
               . foldr hGroup []
               . filter (not . flip member plot)
               . map (north +))
        $ hLines
    downSides =
      sum
        . map
            (length
               . foldr hGroup []
               . filter (not . flip member plot)
               . map (south +))
        $ hLines
    rightSides =
      sum
        . map
            (length
               . foldr vGroup []
               . filter (not . flip member plot)
               . map (east +))
        $ vLines
    leftSides =
      sum
        . map
            (length
               . foldr vGroup []
               . filter (not . flip member plot)
               . map (west +))
        $ vLines
    hLines =
      groupBy (\(V2 _ y0) (V2 _ y1) -> y0 == y1) . sortBy hSort $ plotList
    vLines = groupBy (\(V2 x0 _) (V2 x1 _) -> x0 == x1) . sort $ plotList
    plotList = toList plot

price :: [(Int, Int)] -> Int
price = sum . map (uncurry (*))

part1 :: Bool -> ByteString -> String
part1 _ = show . price . map getAreaPerim . buildGarden . arrayFromByteString

part2 :: Bool -> ByteString -> String
part2 _ = show . price . map getAreaSides . buildGarden . arrayFromByteString
