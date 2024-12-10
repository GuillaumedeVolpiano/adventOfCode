module Day12
  ( part1
  , part2
  ) where

import qualified Control.Applicative        as A (empty)
import           Control.Monad              (void)
import           Data.Either                (fromRight)
import           Data.List                  as L (length)
import           Data.Map                   as M (Map, empty, findWithDefault,
                                                  fromList, insert)
import           Data.Text                  (Text)
import           Data.Vector                as V (Vector, generate, length, (!))
import           Helpers.Parsers.Text       (Parser, string)
import           Text.Megaparsec            (eof, many, manyTill, optional,
                                             parse, try, (<|>))
import           Text.Megaparsec.Char       (char, eol, lowerChar)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, signed,
                                                  space)

data Program = Program
  { getPointer      :: Pointer
  , getRegister     :: Register
  , getInstructions :: Vector Inst
  }

type Register = Map Char Int

type Pointer = Int

type Inst = (Program -> Program)

instance Show Program where
  show (Program p r _) = "Program " ++ show p ++ " " ++ show r

instance Eq Program where
  (Program p1 r1 _) == (Program p2 r2 _) = p1 == p2 && r1 == r2

spaceConsumer :: Parser ()
spaceConsumer = L.space (void . char $ ' ') A.empty A.empty

decimal :: Parser Int
decimal = L.lexeme spaceConsumer L.decimal

signed :: Parser Int
signed = L.signed spaceConsumer decimal

parseProgram :: Parser Program
parseProgram =
  Program 0 M.empty . (\l -> generate (L.length l) (l !!))
    <$> manyTill
          (try parseCPYReg
             <|> parseCPYVal
             <|> parseInc
             <|> parseDec
             <|> try parseJNZReg
             <|> parseJNZVal)
          eof

parseCPYReg :: Parser Inst
parseCPYReg = do
  string "cpy "
  reg1 <- lowerChar
  char ' '
  reg2 <- lowerChar
  optional eol
  return $ \program ->
    let val = findWithDefault 0 reg1 . getRegister $ program
     in movePointer . setRegister program reg2 $ val

parseCPYVal :: Parser Inst
parseCPYVal = do
  string "cpy "
  val <- signed
  reg <- lowerChar
  optional eol
  return $ \program -> movePointer . setRegister program reg $ val

parseInc :: Parser Inst
parseInc = do
  string "inc "
  reg <- lowerChar
  optional eol
  return $ \program ->
    let val = findWithDefault 0 reg . getRegister $ program
     in movePointer . setRegister program reg $ val + 1

parseDec :: Parser Inst
parseDec = do
  string "dec "
  reg <- lowerChar
  optional eol
  return $ \program ->
    let val = findWithDefault 0 reg . getRegister $ program
     in movePointer . setRegister program reg $ val - 1

parseJNZReg :: Parser Inst
parseJNZReg = do
  string "jnz "
  reg <- lowerChar
  char ' '
  offset <- signed
  optional eol
  return $ \program ->
    let val = findWithDefault 0 reg . getRegister $ program
     in jumpPointer program val offset

parseJNZVal :: Parser Inst
parseJNZVal = do
  string "jnz "
  val <- signed
  offset <- signed
  optional eol
  return $ \program -> jumpPointer program val offset

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

execute :: Program -> Int
execute program
  | getPointer program' >= V.length (getInstructions program) =
    findWithDefault 0 'a' . getRegister $ program
  | otherwise = execute program'
  where
    instruction = getInstructions program ! getPointer program
    program' = instruction program

part1 :: Bool -> Text -> String
part1 _ =
  show
    . execute
    . fromRight (error "couldn't parse input")
    . parse parseProgram ""

part2 :: Bool -> Text -> String
part2 _ =
  show
    . execute
    . (\p -> setRegister p 'c' 1)
    . fromRight (error "couldn't parse input")
    . parse parseProgram ""
