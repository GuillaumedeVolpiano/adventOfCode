module Day3
  ( part1
  , part2
  ) where

import           Control.Monad.State        (State, evalState, get, modify)
import           Data.Bifunctor             (second)
import           Data.ByteString            (ByteString, pack)
import           Data.Either                (fromRight)
import           Data.Void                  (Void)
import           Data.Word8                 (_comma, _d, _l, _m, _n, _o,
                                             _parenleft, _parenright,
                                             _quotesingle, _t, _u)
import           Text.Megaparsec            (ParsecT, eof, parse, runParserT,
                                             try, (<|>))
import           Text.Megaparsec.Byte       (char, eol, printChar, string)
import           Text.Megaparsec.Byte.Lexer (decimal)

type Parser = ParsecT Void ByteString (State (Bool, Bool))

parseInput :: Parser Int
parseInput =
  try parseMul
    <|> try
          (do
             string . pack $ [_d, _o, _parenleft, _parenright]
             modify (second . const $ True)
             parseInput)
    <|> try
          (do
             string . pack
               $ [_d, _o, _n, _quotesingle, _t, _parenleft, _parenright]
             modify (second . const $ False)
             parseInput)
    <|> (do
           printChar
           parseInput)
    <|> (do
           eol
           parseInput)
    <|> (do
           eof
           return 0)

parseMul :: Parser Int
parseMul = do
  (isPart1, isDo) <- get
  string . pack $ [_m, _u, _l, _parenleft]
  a <- decimal
  char _comma
  b <- decimal
  char _parenright
  if isPart1 || isDo
    then ((a * b) +) <$> parseInput
    else parseInput

part1 :: Bool -> ByteString -> String
part1 _ =
  show . fromRight 0 . flip evalState (True, True) . runParserT parseInput ""

part2 :: Bool -> ByteString -> String
part2 _ =
  show . fromRight 0 . flip evalState (False, True) . runParserT parseInput ""
