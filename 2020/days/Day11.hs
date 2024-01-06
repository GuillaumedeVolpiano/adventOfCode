module Day11
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed  as A (UArray, bounds, inRange, indices,
                                           (!))
import           Data.HashMap.Strict as M (HashMap, fromList, insert, (!))
import           Data.List           as L (filter, map, unfoldr)
import           Data.Maybe          (Maybe (Just, Nothing), fromJust, isJust)
import           Data.Set            as St (Set, empty, filter, findMax,
                                            fromList, insert, intersection, map,
                                            member, partition, size, union)
import           Helpers.Parsers     (Pos, boolArrayFromString)
import           Linear.V2           (V2 (..))

import           Data.List.Split     (chunksOf)
import           Debug.Trace

type Layout = UArray Pos Bool

type Visible = HashMap Pos (Set Pos)

type Occupied = Set Pos

type Free = Set Pos

type State = (Occupied, Free)

dirs = St.fromList [V2 x y | x <- [-1 .. 1], y <- [-1 .. 1], x /= 0 || y /= 0]

visibleNext :: Layout -> Visible
visibleNext layout =
  M.fromList .
  L.map
    (\x ->
       (x, St.filter (\y -> inRange b y && layout A.! y) . St.map (x +) $ dirs)) .
  L.filter (layout A.!) . indices $
  layout
  where
    b = bounds layout

visibleFirst :: Layout -> Visible
visibleFirst layout =
  M.fromList .
  L.map
    (\x ->
       ( x
       , St.map fromJust . St.filter isJust . St.map (findFirst layout x) $ dirs)) .
  L.filter (layout A.!) . indices $
  layout

findFirst :: Layout -> Pos -> Pos -> Maybe Pos
findFirst layout x y
  | not (inRange b nx) = Nothing
  | layout A.! nx = Just nx
  | otherwise = findFirst layout nx y
  where
    nx = x + y
    b = bounds layout

doRound :: Visible -> Int -> State -> State
doRound visible maxOcc (occupied, free) =
  (filled `union` staid, freed `union` unfilled)
  where
    (filled, unfilled) =
      St.partition (\x -> null (intersection (visible M.! x) occupied)) free
    (freed, staid) =
      St.partition
        (\x -> (>= maxOcc) . size . intersection (visible M.! x) $ occupied)
        occupied

stabilize :: Visible -> Int -> State -> Maybe (State, State)
stabilize visible maxOcc state
  | after == state = Nothing
  | otherwise = Just (after, after)
  where
    after = doRound visible maxOcc state

findStable :: (Layout -> Visible) -> Int -> Layout -> Int
findStable buildVisible maxOcc layout =
  size . fst . last . unfoldr (stabilize visible maxOcc) $ state
  where
    visible = buildVisible layout
    free = St.fromList . L.filter (layout A.!) . indices $ layout
    state = (empty, free)

showState :: State -> String
showState (occupied, free) =
  unlines . chunksOf (mx + 1) $ [rep x y | y <- [0 .. my], x <- [0 .. mx]]
  where
    state = occupied `union` free
    mx = findMax . St.map (\(V2 x _) -> x) $ state
    my = findMax . St.map (\(V2 _ y) -> y) $ state
    rep x y
      | V2 x y `elem` occupied = '#'
      | V2 x y `elem` free = 'L'
      | otherwise = '.'

part1 :: Bool -> String -> String
part1 _ = show . findStable visibleNext 4 . boolArrayFromString 'L'

part2 :: Bool -> String -> String
part2 _ = show . findStable visibleFirst 5 . boolArrayFromString 'L'
