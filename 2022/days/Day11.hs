{-# LANGUAGE FlexibleInstances #-}

module Day11
  ( part1
  , part2
  ) where

import           Data.List.Split    (chunksOf, splitOn)

import           Data.IntMap        as M (IntMap, elems, fromList, insert, keys,
                                          (!))
import           Data.List          (sortBy)
import           Data.Sequence      as S (Seq ((:<|), (:|>)), empty, fromList,
                                          length, null)

data Monkey =
  Monkey
    { items    :: Seq Int
    , operator :: Operation
    , operand  :: Int
    , modulo   :: Int
    , true     :: Int
    , false    :: Int
    , count    :: Int
    }

type Operation = (Int -> Int -> Int)

instance (Eq Operation) where
  a == b = a 1 2 == b 1 2

--instance (Show Operation) where
--  show op
--    | op == (*) = "*"
--    | op == (+) = "+"
--    | op == (^) = "^"
--    | otherwise = "undefined"
parseMonkey :: [String] -> (Int, Monkey)
parseMonkey (num:it:operation:mod:true:false:_) =
  (nm, Monkey items operator operand modulo success fail 0)
  where
    end = last . words
    nm = read . filter (/= ':') . end $ num
    items = S.fromList . map (read . filter (/= ',')) . drop 2 . words $ it
    (operator, operand)
      | (op == "*") && (ope == "old") = ((^), 2)
      | op == "*" = ((*), read ope)
      | op == "+" = ((+), read ope)
      where
        (op:ope:_) = drop 4 . words $ operation
    modulo = read . end $ mod
    success = read . end $ true
    fail = read . end $ false

monkeyTurn :: (Int -> Int) -> IntMap Monkey -> Int -> IntMap Monkey
monkeyTurn worryOp monkeys t = process seq monkeys
  where
    monkey = monkeys ! t
    seq = items monkey
    process curSeq dict
      | S.null curSeq =
        insert
          t
          monkey {items = empty, count = count monkey + S.length seq}
          dict
      | mod itemWorry (modulo monkey) == 0 =
        process is $ insert tmi tm {items = items tm :|> itemWorry} dict
      | otherwise =
        process is $ insert fmi fm {items = items fm :|> itemWorry} dict
      where
        (i :<| is) = curSeq
        itemWorry = worryOp $ operator monkey i $ operand monkey
        tmi = true monkey
        tm = dict ! tmi
        fmi = false monkey
        fm = dict ! fmi

doTurn :: (Int -> Int) -> IntMap Monkey -> IntMap Monkey
doTurn worryOp monkeys = foldl (monkeyTurn worryOp) monkeys . keys $ monkeys

monkeyBusiness :: Int -> (Int -> Int) -> IntMap Monkey -> Int
monkeyBusiness numTurns worryOp =
  product .
  take 2 .
  sortBy (flip compare) .
  map count . elems . last . take (numTurns + 1) . iterate (doTurn worryOp)

monkeys = M.fromList . map parseMonkey . chunksOf 7 . lines

part1 :: Bool -> String -> String
part1 _ = show . monkeyBusiness 20 (`div` 3) . monkeys

part2 :: Bool -> String -> String
part2 _ input = show . monkeyBusiness 10000 (`mod` modOp) $ m
  where
    m = monkeys input
    modOp = product . map modulo . elems $ m
