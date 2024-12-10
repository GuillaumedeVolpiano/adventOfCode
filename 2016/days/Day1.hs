module Day1
  ( part1
  , part2
  ) where

import           Control.Monad.State        (State, evalState, get, put)
import           Data.Char                  (isDigit)
import           Data.Either                (fromRight)
import           Data.Set                   (Set, empty, insert, member)
import           Data.Text                  (Text, pack)
import           Data.Void                  (Void)
import           Helpers.Graph              (Pos, left, manhattanDistance,
                                             north, origin, right)
import           Linear.Vector              ((*^))
import           Text.Megaparsec            (ParsecT, optional, runParserT,
                                             takeWhileP, (<|>))
import           Text.Megaparsec.Char       (eol, string, upperChar)
import           Text.Megaparsec.Char.Lexer (decimal)

type Dir = Pos

type Parser = ParsecT Void Text (State (Pos, Dir))

type ParserTwice = ParsecT Void Text (State (Pos, Dir, Set Pos))

readDir :: Char -> (Dir -> Dir)
readDir 'R' = right
readDir 'L' = left

parseInst :: Parser ()
parseInst = do
  (pos, dir) <- get
  turn <- readDir <$> upperChar
  dist <- decimal
  optional . string . pack $ ", "
  let dir' = turn dir
      pos' = pos + dist *^ dir'
  put (pos', dir')
  return ()

parseInput :: Parser Int
parseInput = do
  parseInst
  end <|> parseInput

parseTwice :: ParserTwice Int
parseTwice = do
  turn <- readDir <$> upperChar
  dist <- decimal
  optional . string . pack $ ", "
  (pos, dir, seen) <- get
  let dir' = turn dir
      pos' = pos + dist *^ dir'
      allPos = map ((pos +) . (*^ dir')) [1 .. dist]
      seen' = foldr insert seen allPos
      result
        | any (`member` seen) allPos =
          return . manhattanDistance origin . head . filter (`member` seen)
            $ allPos
        | otherwise = parseTwice
  put (pos', dir', seen')
  result

end :: Parser Int
end = do
  eol
  (pos, _) <- get
  return $ manhattanDistance origin pos

part1 :: Bool -> Text -> String
part1 _ =
  show . fromRight 0 . flip evalState (origin, north) . runParserT parseInput ""

part2 :: Bool -> Text -> String
part2 _ =
  show
    . fromRight 0
    . flip evalState (origin, north, empty)
    . runParserT parseTwice ""
