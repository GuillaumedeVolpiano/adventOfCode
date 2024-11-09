module Day16
  ( part1
  , part2
  ) where

import           Helpers.Parsers

import           Data.Bifunctor  (second)
import           Data.Bits       ((.&.), (.|.))
import           Data.IntMap     as M (IntMap, fromList, insert, (!))
import           Data.List       as L (filter, foldl', groupBy, sortBy)
import           Data.List.Split (splitOn)
import           Data.Ord        (comparing)
import           Data.Set        as S (Set, difference, elemAt, filter,
                                       fromList, intersection, size)

type Device = IntMap Int

type Translator = IntMap Instruction

data Instruction
  = Addr
  | Addi
  | Mulr
  | Muli
  | Banr
  | Bani
  | Borr
  | Bori
  | Setr
  | Seti
  | Gtir
  | Gtri
  | Gtrr
  | Eqir
  | Eqri
  | Eqrr
  deriving (Show, Eq, Ord)

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

initialDevice = M.fromList [(0, 0), (1, 0), (2, 0), (3, 0)]

execute :: Instruction -> Int -> Int -> Int -> Device -> Device
execute instruction a b c device = insert c (op instruction) device
  where
    op Addr = device ! a + device ! b
    op Addi = device ! a + b
    op Mulr = device ! a * device ! b
    op Muli = device ! a * b
    op Banr = device ! a .&. device ! b
    op Bani = device ! a .&. b
    op Borr = device ! a .|. device ! b
    op Bori = device ! a .|. b
    op Setr = device ! a
    op Seti = a
    op Gtir
      | a > device ! b = 1
      | otherwise = 0
    op Gtri
      | device ! a > b = 1
      | otherwise = 0
    op Gtrr
      | device ! a > device ! b = 1
      | otherwise = 0
    op Eqir
      | a == device ! b = 1
      | otherwise = 0
    op Eqri
      | device ! a == b = 1
      | otherwise = 0
    op Eqrr
      | device ! a == device ! b = 1
      | otherwise = 0

testGroup :: [String] -> (Int, Set Instruction)
testGroup [a, b, c] =
  ( i
  , S.filter
      (\inst -> execute inst x y z initialState == finalState)
      instructions)
  where
    initialState =
      M.fromList . zip [0 ..] . read . dropWhile (/= '[') $ a :: Device
    finalState =
      M.fromList . zip [0 ..] . read . dropWhile (/= '[') $ c :: Device
    [i, x, y, z] = map read . words $ b :: [Int]

runTest :: [String] -> Int
runTest [a, b] = device ! 0
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
