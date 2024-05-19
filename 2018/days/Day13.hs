module Day13
  ( part1
  , part2
  ) where

import           Data.Array.Unboxed as U (UArray, assocs, (!))
import           Data.Either        (fromLeft, fromRight, isLeft, isRight)
import           Data.List          (sort)
import           Data.Map           as M (Map, delete, empty, fromList, insert,
                                          keys, member, notMember, size, (!))
import           Helpers.Parsers    (arrayFromString)
import           Linear.V2          (V2 (..))

import           Debug.Trace

type Carts = Map Pos (Pos, Int)

type Maze = UArray Pos Char

type Pos = V2 Int

crashTick :: Maze -> Carts -> [Pos] -> Either Pos Carts
crashTick maze carts [] = Right carts
crashTick maze carts (x:xs)
  | member newCartPos carts = Left newCartPos
  | otherwise = crashTick maze newCarts xs
  where
    cartVal = carts M.! x
    (newCartPos, newCartVal) = tickCart maze x cartVal
    newCarts = insert newCartPos newCartVal . delete x $ carts

removeTick :: Maze -> Carts -> [Pos] -> Carts
removeTick maze carts [] = carts
removeTick maze carts (x:xs)
  | notMember x carts = removeTick maze carts xs
  | otherwise = removeTick maze newCarts xs
  where
    cartVal = carts M.! x
    (newCartPos, newCartVal) = tickCart maze x cartVal
    newCarts
      | member newCartPos carts = delete x . delete newCartPos $ carts
      | otherwise = insert newCartPos newCartVal . delete x $ carts

tickCart :: Maze -> Pos -> (Pos, Int) -> (Pos, (Pos, Int))
tickCart maze pos (deltaCart@(V2 dx dy), dirCart) =
  (newCartPos, (newDelta, newDir))
  where
    newCartPos = pos + deltaCart
    newDelta
      | maze U.! newCartPos == '\\' = V2 dy dx
      | maze U.! newCartPos == '/' = V2 (-dy) (-dx)
      | maze U.! newCartPos == '+' && dirCart == 0 = V2 dy (-dx)
      | maze U.! newCartPos == '+' && dirCart == 2 = V2 (-dy) dx
      | otherwise = deltaCart
    newDir
      | maze U.! newCartPos == '+' = mod (dirCart + 1) 3
      | otherwise = dirCart

crash :: Maze -> Carts -> Pos
crash maze carts
  | isRight ticked = crash maze . fromRight empty $ ticked
  | isLeft ticked = fromLeft (V2 0 0) ticked
  where
    ticked = crashTick maze carts . sort . keys $ carts

remove :: Maze -> Carts -> Pos
remove maze carts
  | size carts == 1 = head . keys $ carts
  | otherwise = remove maze . removeTick maze carts . sort . keys $ carts

findCarts :: Maze -> Carts
findCarts =
  fromList .
  map (\(a, b) -> (a, (dir b, 0))) .
  filter (\(_, b) -> b `elem` "<>v^") . assocs
  where
    dir '<' = V2 (-1) 0
    dir '>' = V2 1 0
    dir '^' = V2 0 (-1)
    dir 'v' = V2 0 1

part1 :: Bool -> String -> String
part1 _ input = show . crash maze $ carts
  where
    maze = arrayFromString input
    carts = findCarts maze

part2 :: Bool -> String -> String
part2 _ input = show . remove maze $ carts
  where
    maze = arrayFromString input
    carts = findCarts maze
