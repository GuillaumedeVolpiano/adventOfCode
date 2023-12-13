module Day13
  ( part1
  , part2
  ) where

import           Helpers.Parsers    (make2DArray)

import           Data.Array.Unboxed (UArray, bounds, (!))
import           Data.List.Split    (splitWhen)
import           Data.Maybe         (Maybe (Just, Nothing), fromJust, isJust)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

type Pat = UArray Pos Char

data Direction
  = V
  | H
  deriving (Show, Eq, Ord)

findAxis :: Pat -> (Int, Direction)
findAxis pat
  | isJust va = (fromJust va, V)
  | isJust ha = (fromJust ha, H)
  where
    va = findVerticalAxis 0 pat
    ha = findHorizontalAxis 0 pat

-- Our arrays start at 0 where in the problem they start at 1, so we need to add
-- 1 to the result we find
findVerticalAxis :: Int -> Pat -> Maybe Int
findVerticalAxis axis pat
  | axis >= width = Nothing
  | symmetric = Just (axis + 1)
  | otherwise = findVerticalAxis (axis + 1) pat
  where
    (_, V2 width height) = bounds pat
    term = min (axis + 1) (width - axis)
    backward =
      map
        (\x -> map (\y -> pat ! V2 x y) [0 .. height])
        [axis,(axis - 1) .. (axis - term)]
    forward =
      map
        (\x -> map (\y -> pat ! V2 x y) [0 .. height])
        [(axis + 1) .. (axis + term)]
    symmetric = and . zipWith (==) backward $ forward

findHorizontalAxis :: Int -> Pat -> Maybe Int
findHorizontalAxis axis pat
  | axis >= height = Nothing
  | symmetric = Just (axis + 1)
  | otherwise = findHorizontalAxis (axis + 1) pat
  where
    (_, V2 width height) = bounds pat
    term = min (axis + 1) (height - axis)
    backward =
      map
        (\y -> map (\x -> pat ! V2 x y) [0 .. width])
        [axis,(axis - 1) .. (axis - term)]
    forward =
      map
        (\y -> map (\x -> pat ! V2 x y) [0 .. width])
        [(axis + 1) .. (axis + term)]
    symmetric = and . zipWith (==) backward $ forward

findSmudgedAxis :: Pat -> (Int, Direction)
findSmudgedAxis pat
  | isJust va = (fromJust va, V)
  | isJust ha = (fromJust ha, H)
  where
    va = findSmudgedVerticalAxis 0 pat
    ha = findSmudgedHorizontalAxis 0 pat

findSmudgedVerticalAxis :: Int -> Pat -> Maybe Int
findSmudgedVerticalAxis axis pat
  | axis >= width = Nothing
  | quasiSymmetric = Just (axis + 1)
  | otherwise = findSmudgedVerticalAxis (axis + 1) pat
  where
    (_, V2 width height) = bounds pat
    term = min (axis + 1) (width - axis)
    backward =
      map
        (\x -> map (\y -> pat ! V2 x y) [0 .. height])
        [axis,(axis - 1) .. (axis - term)]
    forward =
      map
        (\x -> map (\y -> pat ! V2 x y) [0 .. height])
        [(axis + 1) .. (axis + term)]
    diffPairs = filter (uncurry (/=)) . zip backward $ forward
    justTheOne = length diffPairs == 1
    (bef, aft) = head diffPairs
    smudged = (length . filter (uncurry (/=)) . zip bef $ aft) == 1
    quasiSymmetric = justTheOne && smudged

findSmudgedHorizontalAxis :: Int -> Pat -> Maybe Int
findSmudgedHorizontalAxis axis pat
  | axis >= height = Nothing
  | quasiSymmetric = Just (axis + 1)
  | otherwise = findSmudgedHorizontalAxis (axis + 1) pat
  where
    (_, V2 width height) = bounds pat
    term = min (axis + 1) (height - axis)
    backward =
      map
        (\y -> map (\x -> pat ! V2 x y) [0 .. width])
        [axis,(axis - 1) .. (axis - term)]
    forward =
      map
        (\y -> map (\x -> pat ! V2 x y) [0 .. width])
        [(axis + 1) .. (axis + term)]
    diffPairs = filter (uncurry (/=)) . zip backward $ forward
    justTheOne = length diffPairs == 1
    (bef, aft) = head diffPairs
    smudged = (length . filter (uncurry (/=)) . zip bef $ aft) == 1
    quasiSymmetric = justTheOne && smudged

score :: (Int, Direction) -> Int
score (i, V) = i
score (i, _) = i * 100

part1 :: Bool -> String -> String
part1 _ =
  show .
  sum .
  map (score . findAxis . make2DArray) .
  splitWhen null . map (filter (`elem` "#.")) . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  sum .
  map (score . findSmudgedAxis . make2DArray) .
  splitWhen null . map (filter (`elem` "#.")) . lines
