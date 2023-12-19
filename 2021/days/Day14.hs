{-# LANGUAGE TupleSections #-}

module Day14
  ( part1
  , part2
  ) where

import           Data.List       (groupBy, maximumBy, minimumBy, sortBy)
import           Data.List.Split (splitOn, splitWhen)
import           Data.Map        (Map, fromList, (!))
import           Data.Ord        (comparing)

type Polymer = [((Char, Char), Int)]

type Insertion = Map (Char, Char) [(Char, Char)]

expandPolymer :: (Polymer, Insertion) -> (Polymer, Insertion)
expandPolymer (polymer, insertions) =
  ( groupPolymer . concatMap (\(a, b) -> map (, b) $ insertions ! a) $ polymer
  , insertions)

groupPolymer :: Polymer -> Polymer
groupPolymer =
  map (\x -> (fst . head $ x, sum . map snd $ x)) .
  groupBy (\a b -> fst a == fst b) . sortBy (comparing fst)

parsePolymer :: String -> Polymer
parsePolymer [a]      = []
parsePolymer (a:b:xs) = ((a, b), 1) : parsePolymer (b : xs)

parseInsertion :: [String] -> Insertion
parseInsertion = fromList . map (toTuple . splitOn " -> ")

toTuple :: [String] -> ((Char, Char), [(Char, Char)])
toTuple [a:b:_, c:_] = ((a, b), [(a, c), (c, b)])

parseInput :: [[String]] -> (Polymer, Insertion)
parseInput [a, b] = (groupPolymer . parsePolymer . head $ a, parseInsertion b)

decompose :: Polymer -> [(Char, Int)]
decompose =
  map
    ((\(a, b) ->
        if even b
          then (a, div b 2)
          else (a, div (b + 1) 2)) .
     (\x -> (fst . head $ x, sum . map snd $ x))) .
  groupBy (\a b -> fst a == fst b) .
  sortBy (comparing fst) . concatMap (\((a, b), c) -> [(a, c), (b, c)])

minMax :: Polymer -> Int
minMax polymer =
  (snd . maximumBy (comparing snd) $ grouped) -
  (snd . minimumBy (comparing snd) $ grouped)
  where
    grouped = decompose polymer

part1 :: Bool -> String -> String
part1 _ =
  show .
  minMax .
  fst .
  last . take 11 . iterate expandPolymer . parseInput . splitWhen null . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  minMax .
  fst .
  last . take 41 . iterate expandPolymer . parseInput . splitWhen null . lines
