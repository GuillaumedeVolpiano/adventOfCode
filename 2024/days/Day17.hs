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
import           Data.List            (intersperse)
import           Data.Maybe           (fromJust, mapMaybe)
import           Data.Text            (Text)
import           Helpers.Parsers.Text (signedInts)

import           Debug.Trace

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
  | operand .&. 4 == 0 = operand
  | operand == 4 = register ! a
  | operand == 5 = register ! b
  | operand == 6 = register ! c
  | operand == 7 = error "reserved operand, invalid program"
  | operand > 7 = error "This is supposed to be a three bits program"

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

findVal :: Program -> Int
findVal program =
  minimum . fromJust . reverseEngineer program 0 [] . reverse . getProgram
    $ program

reverseEngineer :: Program -> Int -> [Int] -> [Int] -> Maybe [Int]
reverseEngineer _ result _ codeToGo
  | null codeToGo = Just [result]
reverseEngineer program result codeGone (x:codeToGo)
  | null result' = Nothing
  | otherwise =
    Just
      . concat
      . mapMaybe (\r -> reverseEngineer program r codeGone' codeToGo)
      $ result'
  where
    codeGone' = x : codeGone
    result' =
      filter ((== codeGone') . reverse . getOutput . execute . setA program)
        . map (shiftL result 3 +)
        $ [0 .. 7]

part1 :: Bool -> Text -> String
part1 _ =
  intersperse ','
    . map intToDigit
    . reverse
    . getOutput
    . execute
    . makeProgram
    . signedInts

part2 :: Bool -> Text -> String
part2 _ = show . findVal . makeProgram . signedInts
