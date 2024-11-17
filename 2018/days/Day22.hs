{-# LANGUAGE TupleSections #-}

module Day22
  ( part1
  , part2
  ) where

import           Data.Hashable   (Hashable, hashWithSalt)
import           Data.HashPSQ    as Q (HashPSQ, insert, lookup, minView, null,
                                       singleton)
import           Data.List       (foldl')
import           Data.List.Split (splitOn)
import           Data.Map        as M (Map, elems, insert, notMember, singleton,
                                       (!))
import           Data.Maybe      (mapMaybe)
import           Helpers.Graph   (Pos, east, north, origin, south, west)
import           Helpers.Search  (dijkstraGoalVal)
import           Linear.V2       (V2 (..))

import           Debug.Trace

type Cave = Map Pos ErosionLevel

type ErosionLevel = Int

data Explorer = Explorer
  { pos       :: Pos
  , equipment :: Equipment
  } deriving (Show, Eq, Ord)

data Equipment
  = Neither
  | Climbing
  | Torch
  deriving (Show, Eq, Ord)

instance Hashable Equipment where
  hashWithSalt salt Neither  = salt
  hashWithSalt salt Climbing = 2 * salt
  hashWithSalt salt Torch    = 3 * salt

instance Hashable Explorer where
  hashWithSalt salt (Explorer p e) = hashWithSalt salt p * hashWithSalt salt e

depth test
  | test = 510 -- 510 `mod` 3
  | otherwise = 9171 -- 9171 `mod` 3

target test
  | test = V2 10 10
  | otherwise = V2 7 721

startExplorer = Explorer origin Torch

destExplorer test = Explorer (target test) Torch

expandCave :: Bool -> Cave -> Pos -> Pos -> Cave
expandCave test cave cur@(V2 cx cy) dest@(V2 dx dy)
  | dest == cur = cave
  | otherwise = expandCave test corner (V2 nextX nextY) dest
  where
    addX
      | cx == dx = cave
      | otherwise = foldr (expand test nextX) cave [cy,cy - 1 .. 0]
    addY
      | cy == dy = addX
      | otherwise = foldr (flip (expand test) nextY) addX [cx,cx - 1 .. 0]
    corner
      | cx == dx || cy == dy = addY
      | otherwise = expand test nextX nextY addY
    nextX
      | cx == dx = dx
      | otherwise = cx + 1
    nextY
      | cy == dy = dy
      | otherwise = cy + 1

expand :: Bool -> Int -> Int -> Cave -> Cave
expand test x y cave
  | p == target test = M.insert p 0 cave
  | otherwise = M.insert (V2 x y) el cave
  where
    p = V2 x y
    upper = cave ! V2 x (y - 1)
    left = cave ! V2 (x - 1) y
    el
      | x == 0 = erosionLevel test (48271 * y)
      | y == 0 = erosionLevel test (16807 * x)
      | otherwise = erosionLevel test (upper * left)

erosionLevel :: Bool -> Int -> Int
erosionLevel test gi = (gi + depth test) `mod` 20183

mkCave :: Bool -> Cave
mkCave test = expandCave test (M.singleton origin 0) origin . target $ test

explore ::
     Bool -> Cave -> HashPSQ Explorer Int Explorer -> Map Explorer Int -> Int
explore test cave queue dists
  | Q.null queue = error "target not found"
  | curKey == destExplorer test = estDist
  | otherwise =
    trace
      (show curKey ++ " " ++ show estDist)
      explore
      test
      newCave
      newQueue
      newDists
  where
    Just (curKey, estDist, _, rest) = minView queue
    curPos = pos curKey
    newQueue = foldr (\(e, d) -> Q.insert e d e) rest toConsider
    newDists = foldr (uncurry M.insert) dists toConsider
    nextPos =
      filter (\(V2 x y) -> x >= 0 && y >= 0) . map (curPos +)
        $ [north, south, east, west]
    newCave =
      foldr (\p c -> expandCave test c curPos p) cave
        . filter (`notMember` cave)
        $ nextPos
    toConsider =
      mapMaybe
        consider
        ((switchEquipment, 7)
           : (map (, 1)
                . filter (accessible newCave)
                . map (\x -> curKey {pos = x})
                $ nextPos))
    switchEquipment =
      head
        . filter (\x -> x /= curKey && accessible cave x)
        . map (\e -> curKey {equipment = e})
        $ [Neither, Torch, Climbing]
    consider (e, d)
      | e `notMember` dists || estDist + d < dists ! e = Just (e, estDist + d)
      | otherwise = Nothing

accessible :: Cave -> Explorer -> Bool
accessible cave explorer =
  (equipment explorer == Neither && (cave ! pos explorer) `mod` 3 `elem` [1, 2])
    || (equipment explorer == Torch
          && (cave ! pos explorer) `mod` 3 `elem` [0, 2])
    || (equipment explorer == Climbing
          && (cave ! pos explorer) `mod` 3 `elem` [0, 1])

part1 :: Bool -> String -> String
part1 test _ = show . sum . map (`mod` 3) . elems . mkCave $ test

part2 :: Bool -> String -> String
part2 test _ =
  show
    . explore
        test
        (M.singleton origin 0)
        (Q.singleton startExplorer 0 startExplorer)
    $ M.singleton startExplorer 0
