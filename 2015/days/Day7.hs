module Day7
  ( part1
  , part2
  ) where

import           Data.Bits                  (complement, shiftL, shiftR, (.&.),
                                             (.|.))
import           Data.Char                  (isLower)
import           Data.Either                (fromRight)
import           Data.Map                   (Map, empty, insert, keys, (!))
import qualified Data.Map                   as M (filter)
import           Data.Text                  (Text, pack)
import           Data.Word                  (Word16)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (eof, parse, takeWhile1P, try,
                                             (<|>))
import           Text.Megaparsec.Char       (char, eol, string)
import           Text.Megaparsec.Char.Lexer (decimal)

data Component
  = Gate Op
  | Wire Text
  | Val Word16
  deriving (Show, Eq, Ord)

data Op
  = And Component Component
  | Or Component Component
  | LShift Component Int
  | RShift Component Int
  | Not Component
  deriving (Show, Eq, Ord)

type Circuit = Map Component Component

isVal :: Component -> Bool
isVal (Val _) = True
isVal _       = False

fromVal :: Component -> Word16
fromVal (Val v) = v
fromVal _       = error "Not a Constructor"

isReady :: [Component] -> Component -> Bool
isReady _ (Val v)     = False
isReady cs c@(Wire _) = c `elem` cs
isReady cs (Gate op)  = isReadyOp cs op

isReadyOp :: [Component] -> Op -> Bool
isReadyOp cs (Not x)      = x `elem` cs
isReadyOp cs (And x y)    = (isVal x || x `elem` cs) && y `elem` cs
isReadyOp cs (Or x y)     = x `elem` cs && y `elem` cs
isReadyOp cs (LShift x _) = x `elem` cs
isReadyOp cs (RShift x _) = x `elem` cs

parseInput :: Parser Circuit
parseInput = parseLine <|> (eof >> return empty)

parseLine :: Parser Circuit
parseLine = do
  from <- try parseOp <|> parseWire <|> parseValue
  string . pack $ " -> "
  wire <- takeWhile1P Nothing isLower
  eol
  insert (Wire wire) from <$> parseInput

parseValue :: Parser Component
parseValue = Val <$> decimal

parseWire = Wire <$> takeWhile1P Nothing isLower

parseOp :: Parser Component
parseOp = parseNot <|> parseIntAnd <|> try parseIntOp <|> parseWireOp

parseNot :: Parser Component
parseNot = do
  string . pack $ "NOT "
  Gate . Not . Wire <$> takeWhile1P Nothing isLower

parseIntAnd :: Parser Component
parseIntAnd = do
  value <- decimal
  string . pack $ " AND "
  Gate . And (Val value) . Wire <$> takeWhile1P Nothing isLower

parseIntOp :: Parser Component
parseIntOp = do
  wire <- takeWhile1P Nothing isLower
  op <-
    ((string . pack $ " LSHIFT ") >> return LShift)
      <|> ((string . pack $ " RSHIFT ") >> return RShift)
  Gate . op (Wire wire) <$> decimal

parseWireOp :: Parser Component
parseWireOp = do
  wire1 <- takeWhile1P Nothing isLower
  op <-
    ((string . pack $ " AND ") >> return And)
      <|> ((string . pack $ " OR ") >> return Or)
  Gate . op (Wire wire1) . Wire <$> takeWhile1P Nothing isLower

buildValue :: Circuit -> Word16
buildValue circuit
  | isVal $ circuit ! Wire (pack "a") = fromVal $ circuit ! Wire (pack "a")
  | otherwise = buildValue circuit'
  where
    done = keys . M.filter isVal $ circuit
    ready = keys . M.filter (isReady done) $ circuit
    circuit' = foldr operate circuit ready

operate :: Component -> Circuit -> Circuit
operate k circuit = insert k newVal circuit
  where
    curVal = circuit ! k
    newVal = process curVal
    process w@(Wire _) = circuit ! w
    process (Gate g)   = gateProcess g
    gateProcess (Not w) = Val . complement . val $ w
    gateProcess (LShift w v) = Val . flip shiftL v . val $ w
    gateProcess (RShift w v) = Val . flip shiftR v . val $ w
    gateProcess (And w1 w2)
      | isVal w1 = Val (fromVal w1 .&. val w2)
      | otherwise = Val (val w1 .&. val w2)
    gateProcess (Or w1 w2) = Val (val w1 .|. val w2)
    val w = fromVal $ circuit ! w

rewire :: Circuit -> Word16
rewire circuit = buildValue circuit'
  where
    circuit' = insert (Wire . pack $ "b") (Val . buildValue $ circuit) circuit

part1 :: Bool -> Text -> String
part1 _ =
  show
    . buildValue
    . fromRight (error "parser failed")
    . parse parseInput "day 7"

part2 :: Bool -> Text -> String
part2 _ =
  show . rewire . fromRight (error "parser failed") . parse parseInput "day 7"
