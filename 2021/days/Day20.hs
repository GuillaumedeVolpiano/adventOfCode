module Day20
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, array, indices, (!))
import           Data.List          as L (filter, map)
import           Data.List.Split    (splitOn)
import           Data.Set           as St (Set, filter, fromList, map, member,
                                           size, unions)
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

data Image =
  Image (UArray Int Char) (Set (V2 Int))

neighbours = [V2 x y | y <- [-1 .. 1], x <- [-1 .. 1]]

enhanceEven :: Image -> Image
enhanceEven = enhance (== '#') binEven

enhanceOdd :: Image -> Image
enhanceOdd = enhance (== '.') binOdd

enhanceNoBlink :: Image -> Image
enhanceNoBlink = enhance (== '#') binOdd

binOdd :: Set (V2 Int) -> V2 Int -> Int
binOdd image pos
  | pos `member` image = 1
  | otherwise = 0

binEven :: Set (V2 Int) -> V2 Int -> Int
binEven image = (1 -) . binOdd image

enhance :: (Char -> Bool) -> (Set (V2 Int) -> V2 Int -> Int) -> Image -> Image
enhance selector binariser (Image algo image) = Image algo deBinarized
  where
    toConsider =
      unions . St.map (\p -> St.map (p +) . fromList $ neighbours) $ image
    deBinarized =
      St.filter
        (\p ->
           selector .
           (algo !) .
           foldl (\a b -> b + 2 * a) 0 . L.map (binariser image . (p +)) $
           neighbours)
        toConsider

parseInput :: String -> Image
parseInput = (\[a, b] -> Image (algo a) (image b)) . splitOn "\n\n"
  where
    image b =
      fromList . L.filter (\pos -> (arrayFromString b ! pos) == '#') . indices $
      arrayFromString b
    algo a = array (0, length a - 1) . zip [0 ..] $ a

lit :: Image -> Int
lit (Image _ image) = size image

enhancePair :: Bool -> Image -> Image
enhancePair test
  | test = enhanceNoBlink . enhanceNoBlink
  | otherwise = enhanceEven . enhanceOdd

part1 :: Bool -> String -> String
part1 test = show . lit . enhancePair test . parseInput

part2 :: Bool -> String -> String
part2 test =
  show . lit . last . take 26 . iterate (enhancePair test) . parseInput
