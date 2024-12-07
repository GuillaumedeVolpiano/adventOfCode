module Day1
  ( part1
  , part2
  ) where

import           Data.Char             (isDigit)
import           Data.Either           (fromRight)
import           Data.Set              (Set, empty, insert, member)
import           Data.Text             (Text, pack)
import           Helpers.Graph         (Pos, left, manhattanDistance, north,
                                        origin, right)
import           Helpers.Parsers.Text  (Parser, decimal, string)
import           Linear.Vector         ((*^))
import           Text.Megaparsec       (optional, parse, takeWhileP, (<|>))
import           Text.Megaparsec.Char  (eol, upperChar)

import           Text.Megaparsec.Debug (dbg)

type Dir = Pos

readDir :: Char -> (Dir -> Dir)
readDir 'R' = right
readDir 'L' = left

parseInst :: Pos -> Dir -> Parser (Pos, Dir)
parseInst pos dir = do
  turn <- readDir <$> upperChar
  dist <- decimal
  optional . string $ ", "
  let dir' = turn dir
      pos' = pos + dist *^ dir'
  return (pos', dir')

parseInput :: Pos -> Dir -> Parser Int
parseInput pos dir = do
  (pos', dir') <- parseInst pos dir
  end pos' <|> parseInput pos' dir'

parseTwice :: Pos -> Dir -> Set Pos -> Parser Int
parseTwice pos dir seen = do
  (pos', dir') <- parseInst pos dir
  let seen' = insert pos' seen
      result
        | pos' `member` seen = return . manhattanDistance origin $ pos'
        | otherwise = parseTwice pos' dir' seen'
  result

end :: Pos -> Parser Int
end pos = do
  eol
  return $ manhattanDistance origin pos

part1 :: Bool -> Text -> String
part1 _ = show . fromRight 0 . parse (parseInput origin north) ""

part2 :: Bool -> Text -> String
part2 _ = show . fromRight 0 . parse (parseTwice origin north empty) ""
