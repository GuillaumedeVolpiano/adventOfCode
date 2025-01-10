module Day3
  ( part1
  , part2
  ) where

import           Control.Monad.State  (State, evalState, get, put)
import           Data.Bifunctor       (first)
import           Data.Either          (fromRight)
import           Data.Set             (Set, empty, insert, size, union)
import           Data.Text            (Text)
import           Data.Tuple           (swap)
import           Data.Void            (Void)
import           Helpers.Graph        (Pos, east, north, origin, south, west)
import           Text.Megaparsec      (ParsecT, Token, eof, runParserT, token,
                                       (<|>))
import           Text.Megaparsec.Char (char, eol)

type Parser = ParsecT Void Text (State Pos) Houses

type RobotParser = ParsecT Void Text (State (Pos, Pos)) (Houses, Houses)

type Houses = Set Pos

directions :: Token Text -> Maybe Pos
directions '^' = Just north
directions 'v' = Just south
directions '<' = Just west
directions '>' = Just east

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

part1 :: Bool -> Text -> String
part1 _ =
  show
    . size
    . fromRight (error "parser failed")
    . flip evalState origin
    . runParserT parseInput "Day 3"

part2 :: Bool -> Text -> String
part2 _ =
  show
    . size
    . uncurry union
    . fromRight (error "parser failed")
    . flip evalState (origin, origin)
    . runParserT robotParseDir "Day 3"
