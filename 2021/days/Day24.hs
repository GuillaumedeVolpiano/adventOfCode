module Day24
  ( part1
  , part2
  ) where

import           Control.Monad.State.Lazy (State)
import           Data.List.Split          (chunksOf, splitOn)
import           Data.Map                 (Map, fromList)
import           Data.Maybe               (Maybe (Just, Nothing), isJust)

type Step = (Maybe [Int] -> Int -> Maybe [Int])

data MONAD a =
  MONAD [Step] a

push :: Int -> Maybe [Int] -> Int -> Maybe [Int]
push modifier stack val = (val + modifier :) <$> stack

pop :: Int -> Maybe [Int] -> Int -> Maybe [Int]
pop modifier stack val
  | Just val == fmap ((+ modifier) . head) stack = fmap tail stack
  | otherwise = Nothing

toInst :: [[String]] -> Step
toInst s
  | last (s !! 4) == "1" = push (read . last $ s !! 15)
  | last (s !! 4) == "26" = pop (read . last $ s !! 5)

extendNumbers1 :: [[Int]] -> [[Int]]
extendNumbers1 = concatMap (\x -> map (\y -> x ++ [y]) [9,8 .. 1])

extendNumbers2 :: [[Int]] -> [[Int]]
extendNumbers2 = concatMap (\x -> map (\y -> x ++ [y]) [1 .. 9])

runMonad :: MONAD (Maybe [Int]) -> [Int] -> Maybe [Int]
runMonad (MONAD _ result) []            = result
runMonad (MONAD (s:teps) result) (i:nt) = runMonad (MONAD teps (s result i)) nt

steps :: String -> [Step]
steps = map toInst . chunksOf 18 . map (splitOn " ") . lines

toMonad :: [Step] -> MONAD (Maybe [Int])
toMonad steps = MONAD steps (Just [])

allModels :: [[Int]] -> ([[Int]] -> [[Int]]) -> MONAD (Maybe [Int]) -> [[Int]]
allModels models extendNumbers monad
  | length (head models) == 14 = models
  | otherwise =
    allModels
      (filter (isJust . runMonad monad) . extendNumbers $ models)
      extendNumbers
      monad

part1 :: Bool -> String -> String
part1 _ =
  show .
  foldl (\a b -> b + 10 * a) 0 .
  head . allModels [[]] extendNumbers1 . toMonad . steps

part2 :: Bool -> String -> String
part2 _ =
  show .
  foldl (\a b -> b + 10 * a) 0 .
  head . allModels [[]] extendNumbers2 . toMonad . steps
