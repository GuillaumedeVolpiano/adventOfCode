module Day17
  ( part1
  , part2
  , execute
  , getRegister
  , makeProgram
  ) where

import           Data.Bits            (shiftL, shiftR, testBit, xor, (.&.))
import           Data.Char            (intToDigit)
import           Data.IntMap          (IntMap, fromList, insert, (!))
import           Data.List            (foldl', intersperse)
import           Data.ByteString            (ByteString)
import           Helpers.Parsers.ByteString (signedInts)

data Program = Program
  { getPointer  :: Pointer
  , getRegister :: Register
  , getOutput   :: [Int]
  , getProgram  :: [Int]
  } deriving (Show, Eq)

type Pointer = Int

type Register = IntMap Int

type Operand = Int

type Instruction = Operand -> Program -> Program

instructions :: IntMap Instruction
instructions =
  fromList
    [ (0, adv)
    , (1, bxl)
    , (2, bst)
    , (3, jnz)
    , (4, bxc)
    , (5, out)
    , (6, bdv)
    , (7, cdv)
    ]

a = 0

b = 1

c = 2

setA :: Program -> Int -> Program
setA program val = program {getRegister = insert a val . getRegister $ program}

makeProgram :: [[Int]] -> Program
makeProgram [[aValue], [bValue], [cValue], _, program] =
  Program 0 (fromList [(a, aValue), (b, bValue), (c, cValue)]) [] program

combo :: Register -> Operand -> Int
combo register operand
  | testBit operand 2 = register ! (operand .&. 3)
  | otherwise = operand

movePointer :: Program -> Program
movePointer program = program {getPointer = getPointer program + 2}

adv :: Instruction
adv operand program =
  movePointer
    program
      { getRegister =
          flip (insert a) register
            $ (register ! a) `shiftR` combo register operand
      }
  where
    register = getRegister program

bxl :: Instruction
bxl operand program =
  movePointer
    program
      {getRegister = flip (insert b) register $ (register ! b) `xor` operand}
  where
    register = getRegister program

bst :: Instruction
bst operand program =
  movePointer
    program
      {getRegister = flip (insert b) register $ combo register operand .&. 7}
  where
    register = getRegister program

jnz :: Instruction
jnz operand program = program {getPointer = pointer'}
  where
    pointer = getPointer program
    pointer'
      | getRegister program ! a == 0 = pointer + 2
      | otherwise = operand

bxc :: Instruction
bxc _ program =
  movePointer
    program
      { getRegister =
          flip (insert b) register $ (register ! b) `xor` (register ! c)
      }
  where
    register = getRegister program

out :: Instruction
out operand program =
  movePointer program {getOutput = value : getOutput program}
  where
    register = getRegister program
    value = combo register operand .&. 7

bdv :: Instruction
bdv operand program =
  movePointer
    program
      { getRegister =
          flip (insert b) register
            $ (register ! a) `shiftR` combo register operand
      }
  where
    register = getRegister program

cdv :: Instruction
cdv operand program =
  movePointer
    program
      { getRegister =
          flip (insert c) register
            $ (register ! a) `shiftR` combo register operand
      }
  where
    register = getRegister program

execute :: Program -> Program
execute program
  | getPointer program >= (length . getProgram $ program) = program
  | otherwise = execute $ inst op program
  where
    inst = instructions ! (getProgram program !! getPointer program)
    op = getProgram program !! (getPointer program + 1)

findVal :: Bool -> Program -> Int
findVal test program =
  minimum
    . fst
    . foldl' (reverseEngineer test program) ([0], [])
    . reverse
    . getProgram
    $ program

reverseEngineer :: Bool -> Program -> ([Int], [Int]) -> Int -> ([Int], [Int])
reverseEngineer test program (results, codeGone) val = (results', codeGone')
  where
    codeGone' = val : codeGone
    results' =
      filter ((== codeGone') . engine)
        $ (+) . (`shiftL` 3) <$> results <*> [0 .. 7]
    engine
      | test = getOutput . execute . setA program
      | otherwise = executeSimplified

executeSimplified :: Int -> [Int]
executeSimplified number
  | number == 0 = []
  | otherwise = result : executeSimplified (shiftR number 3)
  where
    result = b'' `mod` 8
    b' = (number `mod` 8) `xor` 1
    c' = number `shiftR` b'
    b'' = b' `xor` c' `xor` 4

part1 :: Bool -> ByteString -> String
part1 test input
  | test =
    intersperse ','
      . map intToDigit
      . reverse
      . getOutput
      . execute
      . makeProgram
      . signedInts
      $ input
  | otherwise =
    intersperse ','
      . map intToDigit
      . executeSimplified
      . head
      . head
      . signedInts
      $ input

part2 :: Bool -> ByteString -> String
part2 test = show . findVal test . makeProgram . signedInts
