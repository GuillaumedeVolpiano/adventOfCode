module Day3
  ( part1
  , part2
  ) where

import           Data.List       (group, sort, transpose)
import           Debug.Trace
import           Helpers.Parsers (alphaNum)

type Bin = [Bool]

toDec :: Bin -> Int
toDec =
  foldl
    (\b a ->
       (if a
          then 1
          else 0) +
       2 * b)
    0

filterOxygen :: [Bin] -> Int -> Bin
filterOxygen list index
  | length list == 1 = head list
  | 2 * length (filter (!! index) list) >= length list =
    filterOxygen (filter (!! index) list) (index + 1)
  | otherwise = filterOxygen (filter (\x -> not $ x !! index) list) (index + 1)

filterCO2 :: [Bin] -> Int -> Bin
filterCO2 list index
  | length list == 1 = head list
  | 2 * length (filter (!! index) list) >= length list =
    filterCO2 (filter (\x -> not $ x !! index) list) (index + 1)
  | otherwise = filterCO2 (filter (!! index) list) (index + 1)

rate2 :: [Bin] -> Int
rate2 bin = (toDec . filterOxygen bin $ 0) * (toDec . filterCO2 bin $ 0)

toRate1 :: Bin -> Int
toRate1 bin = toDec bin * toDec (map not bin)

rate1 :: [Bin] -> Int
rate1 =
  toRate1 . map ((\(a:b:_) -> length b > length a) . group . sort) . transpose

readBin :: String -> [Bin]
readBin = map (map (== '1') . concat) . alphaNum

part1 :: Bool -> String -> String
part1 _ = show . rate1 . readBin

part2 :: Bool -> String -> String
part2 _ = show . rate2 . readBin
