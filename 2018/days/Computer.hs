module Computer
  ( Device
  , Instruction(..)
  , execute
  ) where

import           Data.Bits   ((.&.), (.|.))
import           Data.IntMap as M (IntMap, insert, (!))

type Device = IntMap Int

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
