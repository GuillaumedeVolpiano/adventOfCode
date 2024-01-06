module Day13
  ( part1
  , part2
  ) where

import           Data.Bifunctor                   (second)
import           Data.List                        (minimumBy)
import           Data.List.Split                  (splitOn)
import           Data.Maybe                       (fromJust)
import           Math.NumberTheory.Moduli.Chinese (chinese)

findBus :: [[Int]] -> Int
findBus [[timestamp], timetable] =
  (\x -> x * (x - mod timestamp x)) .
  minimumBy (\a b -> compare (a - mod timestamp a) (b - mod timestamp b)) $
  timetable

findTimeStamp :: [(Integer, Integer)] -> Integer
findTimeStamp (h:ead) =
  (\(a, b) -> mod (b - a) b) . foldl (\a -> fromJust . chinese a) h $ ead

part1 :: Bool -> String -> String
part1 _ =
  show . findBus . map (map read . filter (/= "x") . splitOn ",") . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  findTimeStamp .
  map (second read) .
  filter ((/= "x") . snd) . zip [0 ..] . splitOn "," . last . lines
