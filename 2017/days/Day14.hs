module Day14
  ( part1
  , part2
  ) where

import           Data.Graph.Inductive.Query.DFS (components)
import           Helpers.Graph                  (Gr, Pos, assocsToGraph, east,
                                                 north, south, west)
import           KnotHash                       (fullHash)
import           Linear.V2                      (V2 (..))

bitify :: Int -> [Int]
bitify x = [div x 8, div (x `mod` 8) 4, div (x `mod` 4) 2, x `mod` 2]

buildRows :: String -> [[Int]]
buildRows input =
  map (\x -> concatMap bitify . fullHash $ input ++ ('-' : show x)) [0 .. 127]

buildGraph :: String -> Gr Pos ()
buildGraph input = assocsToGraph assocs
  where
    pos =
      concatMap (map fst . filter ((== 1) . snd))
        . zipWith (\y r -> map (\(x, v) -> (V2 x y, v)) r) [0 ..]
        . map (zip [0 ..])
        . buildRows
        $ input
    assocs = map (\x -> (x, adjacent x)) pos
    adjacent p =
      filter (flip elem pos . fst) . map (\x -> (p + x, ()))
        $ [north, south, east, west]

part1 :: Bool -> String -> String
part1 _ = show . length . concatMap (filter (== 1)) . buildRows

part2 :: Bool -> String -> String
part2 _ = show . length . components . buildGraph
