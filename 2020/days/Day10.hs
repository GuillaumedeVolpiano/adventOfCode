module Day10
  ( part1
  , part2
  ) where

import           Data.List  (group, sort)
import           Data.Maybe (Maybe (Just, Nothing), catMaybes, fromJust,
                             isNothing)

countDifs :: [Int] -> Int
countDifs adapters =
  (\[a, b] -> (a + 1) * (b + 1)) .
  map length . group . sort . zipWith (flip (-)) (init sorted) $
  tail sorted
  where
    sorted = sort adapters

combinations :: [Int] -> Int -> Maybe Int
combinations _ (-2) = Just 0
combinations _ (-1) = Just 0
combinations _ 0 = Just 1
combinations list n
  | n `elem` list && all isNothing combined = Nothing
  | n `elem` list = Just . sum . catMaybes $ combined
  | otherwise = Nothing
  where
    combined = map (combinations list . (n -)) [1 .. 3]

memCombinations :: [Int] -> Int -> Maybe Int
memCombinations list = (map comb [0 ..] !!)
  where
    comb 0 = Just 0
    comb 1 = combinations list 1
    comb 2 = combinations list 2
    comb n
      | n `elem` list && all isNothing (combined n) = Nothing
      | n `elem` list = Just . sum . catMaybes $ combined n
      | otherwise = Nothing
    combined n = map (memCombinations list . (n -)) [1 .. 3]

findCombinations :: [Int] -> Int
findCombinations list = fromJust $ memCombinations sorted device
  where
    sorted = sort list
    device = last sorted

part1 :: Bool -> String -> String
part1 _ = show . countDifs . map read . lines

part2 :: Bool -> String -> String
part2 _ = show . findCombinations . map read . lines
