module Day12
  ( part1
  , part2
  ) where

import           Data.List.Split    (splitOn)

import           Data.Array.Unboxed as A (Array, array, bounds, inRange,
                                          indices, (!))
import           Data.Char          (ord)
import           Data.Map           as M (Map, empty, insert, notMember, (!))
import           Data.Sequence      as S (Seq ((:<|), (:|>)), null, singleton)
import           Linear.V2          (V2 (..))

type Pos = V2 Int

data State =
  State (Array Pos Char) (Seq Pos) (Map Pos Int)

day = 12

up = V2 0 (-1)

down = V2 0 1

left = V2 (-1) 0

right = V2 1 0

directions = [up, down, left, right]

bFS :: State -> Int
bFS (State elevMap queue dists)
  | S.null queue = 1000000
  | elevMap A.! q == 'E' = dist
  | otherwise = bFS (State elevMap newQueue newDists)
  where
    (q :<| qs) = queue
    dist = dists M.! q
    nextPos =
      filter (\x -> accessible x && notMember x dists) . map (q +) $ directions
    newQueue = foldl (:|>) qs nextPos
    newDists = foldl (\x y -> insert y (dist + 1) x) dists nextPos
    accessible np =
      inRange (bounds elevMap) np && heightDif (elevMap A.! q) (elevMap A.! np)
    heightDif c d
      | c == 'S' = d == 'a' || d == 'b'
      | d == 'E' = c == 'y' || c == 'z'
      | d == 'S' = True
      | otherwise = ord d - ord c <= 1

elevMap :: Int -> Int -> [String] -> Array Pos Char
elevMap width height lined =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, lined !! y !! x) | x <- [0 .. width], y <- [0 .. height]]

borders width height =
        filter (\(V2 x y) -> x == 0 || y == 0 || x == width || y == height) .
        indices

part1 :: Bool -> String -> String
part1 _ input = show . bFS $ State em (singleton startPos) (insert startPos 0 empty)
  where
    em = elevMap width height lined
    b = borders width height em
    startPos =  head $ filter (\x -> em A.! x == 'S') b
    lined = lines input
    height = length lined - 1
    width = (length . head $ lined) - 1

part2 :: Bool -> String -> String
part2 _ input = show . minimum . map (\x -> bFS (State em (singleton x) (insert x 0 empty))) $ allStartPos
  where
    em = elevMap width height lined
    b = borders width height em  
    allStartPos =
        filter (\x -> em A.! x == 'S' || em A.! x == 'a') b
    lined = lines input
    height = length lined - 1
    width = (length . head $ lined) - 1
