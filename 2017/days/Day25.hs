module Day25
  ( part1
  , part2
  ) where

import           Data.Either          (fromRight)
import           Data.IntSet          as S (IntSet, delete, empty, insert,
                                            member, size)
import           Data.Map             as M (Map, empty, fromList, (!))
import           Helpers.Parsers      (Parser)
import           Text.Megaparsec      (eof, many, manyTill, optional, parse,
                                       sepBy, (<|>))
import           Text.Megaparsec.Char (char, eol, printChar, string)

type State = Char

type Value = (Int -> IntSet -> IntSet)

type Dir = Int

type Inst = ((Value, Dir, State), (Value, Dir, State))

type Machine = (State, Pointer, Tape)

type Program = Map Char Inst

type Tape = IntSet

type Pointer = Int

consumeWord :: Parser ()
consumeWord = do
  manyTill printChar (char ' ' <|> char '.' <|> char ':')
  return ()

consumeSpace :: Parser ()
consumeSpace = do
  char ' '
  return ()

consumeLine :: Parser ()
consumeLine = do
  manyTill (consumeSpace <|> consumeWord) eol
  return ()

relevant :: Parser String
relevant = do
  result <-
    last <$> many (manyTill printChar (char ' ' <|> char ':' <|> char '.'))
  optional eol
  return result

parseInput :: Parser (Int, Program)
parseInput = do
  consumeLine
  steps <- parseSteps
  eol
  insts <- fromList <$> many parseInst
  return (steps, insts)

parseSteps :: Parser Int
parseSteps = do
  read . last . init
    <$> manyTill (manyTill printChar (char ' ' <|> char '.')) eol

parseInst :: Parser (State, Inst)
parseInst = do
  state <- head <$> relevant
  false <- opt
  true <- opt
  nextState <- head <$> relevant
  return (state, (false, true))

opt :: Parser (Value, Dir, State)
opt = do
  consumeLine
  val <-
    (\x ->
       if x == "1"
         then insert
         else delete)
      <$> relevant
  dir <-
    (\x ->
       if x == "right"
         then 1
         else (-1))
      <$> relevant
  state <- head <$> relevant
  return (val, dir, state)

step :: Program -> Machine -> Machine
step program (state, pointer, tape) = (state', pointer', tape')
  where
    (false, true) = program ! state
    (value, dir, state')
      | pointer `member` tape = true
      | otherwise = false
    pointer' = pointer + dir
    tape' = value pointer tape

checksum :: Machine -> Int
checksum (_, _, tape) = size tape

part1 :: Bool -> String -> String
part1 _ input =
  show . checksum . (!! nSteps) . iterate (step program) $ ('A', 0, S.empty)
  where
    (nSteps, program) = fromRight (0, M.empty) . parse parseInput "" $ input

part2 :: Bool -> String -> String
part2 _ _ = "Part 2"
