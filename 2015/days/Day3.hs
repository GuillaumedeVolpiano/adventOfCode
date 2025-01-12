module Day3
  ( part1
  , part2
  ) where

import           Control.Monad.State  (State, evalState, get, put)
import           Data.Bifunctor       (first)
import           Data.ByteString      (ByteString)
import           Data.Either          (fromRight)
import           Data.Set             (Set, empty, insert, size, union)
import           Data.Tuple           (swap)
import           Data.Void            (Void)
import           Data.Word8           (_circum, _greater, _less, _v)
import           Helpers.Graph        (Pos, east, north, origin, south, west)
import           Text.Megaparsec      (ParsecT, Token, eof, runParserT, token,
                                       (<|>))
import           Text.Megaparsec.Char (char, eol)

type Parser = ParsecT Void ByteString (State Pos) Houses

type RobotParser = ParsecT Void ByteString (State (Pos, Pos)) (Houses, Houses)

type Houses = Set Pos

directions :: Token ByteString -> Maybe Pos
directions d
  | d == _circum = Just north
  | d == _v = Just south
  | d == _less = Just west
  | d == _greater = Just east

parseDir :: Parser
parseDir = do
  pos <- get
  dir <- token directions empty
  put (pos + dir)
  insert (pos + dir) <$> parseInput

robotParseDir :: RobotParser
robotParseDir = do
  (curPos, otherPos) <- get
  dir <- token directions empty
  put (otherPos, curPos + dir)
  swap . first (insert (curPos + dir)) <$> robotParseInput

parseInput :: Parser
parseInput = parseDir <|> (eof >> return empty)

robotParseInput :: RobotParser
robotParseInput = robotParseDir <|> (eof >> return (empty, empty))

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . size
    . fromRight (error "parser failed")
    . flip evalState origin
    . runParserT parseInput "Day 3"

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . size
    . uncurry union
    . fromRight (error "parser failed")
    . flip evalState (origin, origin)
    . runParserT robotParseDir "Day 3"
