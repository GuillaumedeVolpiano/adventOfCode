module Day17
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed (UArray, indices, (!))
import           Data.Hashable      (Hashable)
import           Data.HashSet       as St (HashSet, difference, empty, filter,
                                           foldl', fromList, intersection, map,
                                           size, union)
import           Data.List          as L (filter, map)
import           Helpers.Parsers    (boolArrayFromString)
import           Linear.V2          (V2 (..))
import           Linear.V3          (V3 (..))
import           Linear.V4          (V4 (..))
import           Linear.Vector      (Additive)

type Pos = V3 Int

neighbours3D =
  fromList
    [ V3 x y z
    | x <- [-1 .. 1]
    , y <- [-1 .. 1]
    , z <- [-1 .. 1]
    , x /= 0 || y /= 0 || z /= 0
    ]

neighbours4D =
  fromList
    [ V4 w x y z
    | w <- [(-1) .. 1]
    , x <- [(-1) .. 1]
    , y <- [(-1) .. 1]
    , z <- [(-1) .. 1]
    , w /= 0 || x /= 0 || y /= 0 || z /= 0
    ] :: HashSet (V4 Int)

gameOfLife ::
     (Additive a, Hashable (a Int), Num (a Int))
  => HashSet (a Int)
  -> HashSet (a Int)
  -> HashSet (a Int)
gameOfLife neighb alive = stillAlive `union` bornAgain
  where
    stillAlive = St.filter (\x -> numNeighbours x `elem` [2, 3]) alive
    bornAgain = St.filter (\x -> numNeighbours x == 3) deadToConsider
    numNeighbours = size . intersection alive . vicinity
    vicinity x = St.map (x +) neighb
    toConsider = foldl' union empty $ St.map vicinity alive
    deadToConsider = difference toConsider alive

extractLiving3D :: UArray (V2 Int) Bool -> HashSet (V3 Int)
extractLiving3D slice =
  fromList . L.map (\(V2 x y) -> V3 x y 0) . L.filter (slice !) . indices $
  slice

extractLiving4D :: UArray (V2 Int) Bool -> HashSet (V4 Int)
extractLiving4D slice =
  fromList . L.map (\(V2 x y) -> V4 x y 0 0) . L.filter (slice !) . indices $
  slice

part1 :: Bool -> String -> String
part1 _ =
  show .
  size .
  last .
  take 7 .
  iterate (gameOfLife neighbours3D) . extractLiving3D . boolArrayFromString '#'

part2 :: Bool -> String -> String
part2 _ =
  show .
  size .
  last .
  take 7 .
  iterate (gameOfLife neighbours4D) . extractLiving4D . boolArrayFromString '#'
