module Day16
  ( part1
  , part2
  ) where

import           Computer        (Device, Instruction (..), createDevice,
                                  deviceFromList, execute, register)
import           Helpers.Parsers

import           Data.Bifunctor  (second)
import           Data.IntMap     as M (IntMap, fromList, insert, (!))
import           Data.List       as L (filter, foldl', groupBy, sortBy)
import           Data.List.Split (splitOn)
import           Data.Ord        (comparing)
import           Data.Set        as S (Set, difference, elemAt, filter,
                                       fromList, intersection, size)

type Translator = IntMap Instruction

instructions =
  S.fromList
    [ Addr
    , Addi
    , Mulr
    , Muli
    , Banr
    , Bani
    , Borr
    , Bori
    , Setr
    , Seti
    , Gtir
    , Gtri
    , Gtrr
    , Eqir
    , Eqri
    , Eqrr
    ]

initialDevice = createDevice 4

testGroup :: [String] -> (Int, Set Instruction)
testGroup [a, b, c] =
  ( i
  , S.filter
      (\inst -> execute inst x y z initialState == finalState)
      instructions)
  where
    initialState = deviceFromList . read . dropWhile (/= '[') $ a
    finalState = deviceFromList . read . dropWhile (/= '[') $ c
    [i, x, y, z] = map read . words $ b :: [Int]

runTest :: [String] -> Int
runTest [a, b] = register 0 device
  where
    device =
      foldl' (process instructMap) initialDevice
        . map (map read . words)
        . lines
        $ b
    groups =
      map (foldr (\(i, a) (_, b) -> (i, a `intersection` b)) (0, instructions))
        . groupBy (\a b -> fst a == fst b)
        . sortBy (comparing fst)
        . map (testGroup . lines)
        . splitOn "\n\n"
        $ a
    instructMap =
      M.fromList
        . reduce
            (map (second (elemAt 0)) . L.filter ((== 1) . size . snd) $ groups)
        $ L.filter ((> 1) . size . snd) groups

process :: Translator -> Device -> [Int] -> Device
process translator device [inst, a, b, c] =
  execute (translator ! inst) a b c device
process _ device [] = device
process _ _ inst = error (show inst ++ " is not formated as expected")

reduce ::
     [(Int, Instruction)] -> [(Int, Set Instruction)] -> [(Int, Instruction)]
reduce a [] = a
reduce insts instLists = reduce newInsts newInstList
  where
    newInsts = insts ++ discovered
    newInstList = L.filter ((> 1) . size . snd) reduced
    known = S.fromList . map snd $ insts
    reduced = map (second (`difference` known)) instLists
    discovered =
      map (second (elemAt 0)) . L.filter ((== 1) . size . snd) $ reduced

part1 :: Bool -> String -> String
part1 _ =
  show
    . length
    . L.filter (>= 3)
    . map (size . snd . testGroup . lines)
    . splitOn "\n\n"
    . head
    . splitOn "\n\n\n"

part2 :: Bool -> String -> String
part2 _ = show . runTest . splitOn "\n\n\n"
