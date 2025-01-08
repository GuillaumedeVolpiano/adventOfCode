module Assembunny
  ( parseProgram
  , decompile
  , execute
  , setRegister
  ) where

import qualified Control.Applicative        as A (empty)
import           Control.Monad              (void)
import           Data.Map                   (Map, findWithDefault, fromList,
                                             insert)
import qualified Data.Map                   as M (empty)
import           Data.Vector                (Vector, generate, imap, toList,
                                             (!))
import qualified Data.Vector                as V (length)
import           Helpers.Parsers.Text       (Parser, string)
import           Text.Megaparsec            (eof, many, manyTill, optional, try,
                                             (<|>))
import           Text.Megaparsec.Char       (char, eol, lowerChar)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, signed,
                                                  space)

data Program = Program
  { getPointer      :: Pointer
  , getRegister     :: Register
  , getInstructions :: Vector Instruction
  }

data Instruction
  = CPYVal Int Char
  | CPYReg Char Char
  | Inc Char
  | Dec Char
  | JnzReg Char Int
  | JnzVal Int Int
  | JnzValReg Int Char
  | TglVal Int
  | TglReg Char
  deriving (Eq, Ord, Show)

type Register = Map Char Int

type Pointer = Int

type Inst = (Program -> Program)

instance Show Program where
  show (Program p r _) = "Program " ++ show p ++ " " ++ show r

instance Eq Program where
  (Program p1 r1 _) == (Program p2 r2 _) = p1 == p2 && r1 == r2

prettyPrint :: Int -> Instruction -> String
prettyPrint line (CPYVal val reg) =
  show line ++ ": set register " ++ reg : " to val " ++ show val
prettyPrint line (CPYReg reg1 reg2) =
  show line
    ++ ": set register "
    ++ reg2
    : " to the value of register "
    ++ reg1
    : ""
prettyPrint line (Inc reg) =
  show line ++ ": increase register " ++ reg : " by 1"
prettyPrint line (Dec reg) =
  show line ++ ": decrease register " ++ reg : " by 1"
prettyPrint line (JnzReg reg offset) =
  show line
    ++ ": if register "
    ++ reg
    : " is not 0 then jump to line "
    ++ show (line + offset)
prettyPrint line (JnzVal val offset) =
  show line
    ++ ": if value "
    ++ show val
    ++ " is not 0 then jump to line "
    ++ show (line + offset)
prettyPrint line (TglVal offset) =
  show line ++ ": toggle instruction at line " ++ show (line + offset)
prettyPrint line (TglReg reg) =
  show line ++ ": toggle instruction based on value of register " ++ reg : ""
prettyPrint line (JnzValReg val reg) =
  show line
    ++ ": if "
    ++ show val
    ++ " is not 0 then jump based on value of register "
    ++ reg
    : ""

spaceConsumer :: Parser ()
spaceConsumer = L.space (void . char $ ' ') A.empty A.empty

decimal :: Parser Int
decimal = L.lexeme spaceConsumer L.decimal

signed :: Parser Int
signed = L.signed spaceConsumer decimal

parseProgram :: Parser Program
parseProgram =
  Program 0 M.empty . (\l -> generate (length l) (l !!))
    <$> manyTill
          (try parseCPYReg
             <|> parseCPYVal
             <|> parseInc
             <|> parseDec
             <|> try parseJnzReg
             <|> try parseJnzVal
             <|> parseJnzValReg
             <|> try parseTglVal
             <|> parseTglReg)
          eof

parseCPYReg :: Parser Instruction
parseCPYReg = do
  string "cpy "
  reg1 <- lowerChar
  char ' '
  reg2 <- lowerChar
  optional eol
  return $ CPYReg reg1 reg2

parseCPYVal :: Parser Instruction
parseCPYVal = do
  string "cpy "
  val <- signed
  reg <- lowerChar
  optional eol
  return $ CPYVal val reg

parseInc :: Parser Instruction
parseInc = do
  string "inc "
  reg <- lowerChar
  optional eol
  return $ Inc reg

parseDec :: Parser Instruction
parseDec = do
  string "dec "
  reg <- lowerChar
  optional eol
  return $ Dec reg

parseJnzReg :: Parser Instruction
parseJnzReg = do
  string "jnz "
  reg <- lowerChar
  char ' '
  offset <- signed
  optional eol
  return $ JnzReg reg offset

parseJnzVal :: Parser Instruction
parseJnzVal = do
  string "jnz "
  val <- signed
  offset <- signed
  optional eol
  return $ JnzVal val offset

parseJnzValReg :: Parser Instruction
parseJnzValReg = do
  string "jnz "
  val <- signed
  offset <- lowerChar
  optional eol
  return $ JnzValReg val offset

parseTglVal :: Parser Instruction
parseTglVal = do
  string "tgl "
  val <- signed
  optional eol
  return $ TglVal val

parseTglReg :: Parser Instruction
parseTglReg = do
  string "tgl "
  reg <- lowerChar
  optional eol
  return $ TglReg reg

setRegister :: Program -> Char -> Int -> Program
setRegister (Program p r i) ad v = Program p r' i
  where
    r' = insert ad v r

jumpPointer :: Program -> Int -> Int -> Program
jumpPointer program val offset
  | val /= 0 = program {getPointer = getPointer program + offset}
  | otherwise = movePointer program

movePointer :: Program -> Program
movePointer program = program {getPointer = 1 + getPointer program}

runInstruction :: Instruction -> Program -> Program
runInstruction instruction program = program' instruction
  where
    register = getRegister program
    val (CPYReg reg _) = findWithDefault 0 reg register
    val (CPYVal v _)   = v
    val (Inc reg)      = findWithDefault 0 reg register + 1
    val (Dec reg)      = findWithDefault 0 reg register - 1
    val (JnzReg reg _) = findWithDefault 0 reg register
    val (JnzVal val _) = val
    reg (CPYReg _ reg) = reg
    reg (CPYVal _ reg) = reg
    reg (Inc reg)      = reg
    reg (Dec reg)      = reg
    offset (JnzVal _ off) = off
    offset (JnzReg _ off) = off
    register' = insert (reg instruction) (val instruction) register
    program' (JnzVal _ _)
      | val instruction /= 0 = offsetPointer
      | otherwise = advancePointer program
    program' (JnzReg _ _)
      | val instruction /= 0 = offsetPointer
      | otherwise = advancePointer program
    program' _ = advancePointer program {getRegister = register'}
    offsetPointer = program {getPointer = getPointer program + val instruction}

advancePointer :: Program -> Program
advancePointer program = program {getPointer = getPointer program + 1}

execute :: Program -> Int
execute program
  | getPointer program' >= V.length (getInstructions program) =
    findWithDefault 0 'a' . getRegister $ program
  | otherwise = execute program'
  where
    instruction = getInstructions program ! getPointer program
    program' = runInstruction instruction program

decompile :: Program -> String
decompile = unlines . toList . imap prettyPrint . getInstructions
