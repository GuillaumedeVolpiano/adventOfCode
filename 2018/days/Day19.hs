module Day19
  ( part1
  , part2
  ) where

import           Computer             (Device, Instruction, createDevice,
                                       execute, pExecute, register,
                                       setPregister, setRegister)
import           Data.Char            (toUpper)
import           Helpers.Parsers      (Parser, alpha, numbers, nums)
import           Text.Megaparsec      (eof, manyTill, optional, parse)
import           Text.Megaparsec.Char (char, eol, string)

-- Lines 17 onwards of the program create a big, or very big, number. Lines 1-15
-- iterate through all numbers lower than that number and check whether their
-- product is equal to it, in which case the first factor is added to register
-- 0. In other terms, the program sums all the factors of the initial number.
ri1 = (2 ^ 2) * 19 * 11 + 4 * 22 + 5

ri2 = ri1 + (27 * 28 + 29) * 30 * 14 * 32

-- parseInput :: Parser (Device, [Device -> Device])
-- parseInput = do
--   string "#ip "
--   (Just reg) <- nums
--   eol
--   instructions <- manyTill parseInstruction eof
--   let device = setPregister reg . createDevice $ 6
--   return (device, instructions)
--
-- parseInstruction :: Parser (Device -> Device)
-- parseInstruction = do
--   (Just (r:awInst)) <- alpha
--   char ' '
--   (Just a) <- nums
--   char ' '
--   (Just b) <- nums
--   char ' '
--   (Just c) <- nums
--   optional eol
--   let inst = read (toUpper r : awInst)
--   return $ execute inst a b c
--
findAllFactors :: Int -> Int -> [Int]
findAllFactors target cur
  | cur ^ 2 > target = []
  | cur ^ 2 == target = [cur]
  | mod target cur == 0 = cur : div target cur : findAllFactors target (cur + 1)
  | otherwise = findAllFactors target (cur + 1)

part1 :: Bool -> String -> String
part1 _ _ = show . sum . findAllFactors ri1 $ 1
  -- show . register 0 . pExecute instructions $ device
  -- where
    -- Right (device, instructions) = parse parseInput "" input

part2 :: Bool -> String -> String
part2 _ _ = show . sum . findAllFactors ri2 $ 1
--  show . register 0 . pExecute instructions . setRegister 0 1 $ device
  -- where
    -- Right (device, instructions) = parse parseInput "" input
