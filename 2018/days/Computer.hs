{-# LANGUAGE TupleSections #-}

module Computer
  ( Device
  , Instruction(..)
  , createDevice
  , deviceFromList
  , execute
  , pExecute
  , register
  , setPregister
  , setRegister
  ) where

import           Data.Bits   ((.&.), (.|.))
import           Data.IntMap as M (IntMap, fromList, insert, notMember, (!))
import           Data.Maybe  (isNothing)

data Device = Device
  { pregister :: Maybe Int
  , pvalue    :: Int
  , memory    :: Register
  } deriving (Show, Eq)

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
  deriving (Show, Read, Eq, Ord)

type Register = IntMap Int

createDevice :: Int -> Device
createDevice registers =
  Device Nothing 0 . fromList . map (, 0) $ [0 .. (registers - 1)]

deviceFromList :: [Int] -> Device
deviceFromList = Device Nothing 0 . fromList . zip [0 ..]

pExecute :: [Device -> Device] -> Device -> Device
pExecute instructions device
  | pvalue device >= 0 && pvalue device < length instructions =
    pExecute instructions . postExec . (instructions !! pvalue device) . preExec
      $ device
  | otherwise = device

preExec :: Device -> Device
preExec device = device {memory = insert pr pv mem}
  where
    Just pr = pregister device
    pv = pvalue device
    mem = memory device

postExec :: Device -> Device
postExec device = device {pvalue = (mem ! pr) + 1}
  where
    mem = memory device
    Just pr = pregister device

execute :: Instruction -> Int -> Int -> Int -> Device -> Device
execute instruction a b c device =
  device {memory = insert c (op instruction) mem}
  where
    mem = memory device
    op Addr = mem ! a + mem ! b
    op Addi = mem ! a + b
    op Mulr = mem ! a * mem ! b
    op Muli = mem ! a * b
    op Banr = mem ! a .&. mem ! b
    op Bani = mem ! a .&. b
    op Borr = mem ! a .|. mem ! b
    op Bori = mem ! a .|. b
    op Setr = mem ! a
    op Seti = a
    op Gtir
      | a > mem ! b = 1
      | otherwise = 0
    op Gtri
      | mem ! a > b = 1
      | otherwise = 0
    op Gtrr
      | mem ! a > mem ! b = 1
      | otherwise = 0
    op Eqir
      | a == mem ! b = 1
      | otherwise = 0
    op Eqri
      | mem ! a == b = 1
      | otherwise = 0
    op Eqrr
      | mem ! a == mem ! b = 1
      | otherwise = 0

register :: Int -> Device -> Int
register r = flip (!) r . memory

setRegister :: Int -> Int -> Device -> Device
setRegister r v d = d{memory = insert r v . memory $ d}

setPregister :: Int -> Device -> Device
setPregister r d = d {pregister = Just r}
