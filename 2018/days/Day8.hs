module Day8
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Char            (isDigit)
import           Data.Either          (fromRight)
import           Helpers.Parsers      (Parser)
import           Text.Megaparsec      (count, optional, parse, takeWhile1P)
import           Text.Megaparsec.Char (char)

data Tree =
  Node Header [Tree] Metadata
  deriving (Show)

type Header = (Int, Int)

type Metadata = [Int]

parseNode :: Parser Tree
parseNode = do
  nChildren <- parseNumber
  nMetadata <- parseNumber
  children <- count nChildren parseNode
  metadata <- count nMetadata parseNumber
  return (Node (nChildren, nMetadata) children metadata)

parseNumber :: Parser Int
parseNumber = do
  number <- read <$> takeWhile1P Nothing isDigit
  void . optional . char $ ' '
  return number

sumMetadata :: Tree -> Int
sumMetadata (Node _ subTree metadata) =
  sum metadata + sum (map sumMetadata subTree)

value :: Tree -> Int
value (Node (nc, _) subTree metadata)
  | nc == 0 = sum metadata
  | otherwise = sum . map extVal $ metadata
  where
    extVal x
      | x > nc = 0
      | otherwise = value (subTree !! (x - 1))

part1 :: Bool -> String -> String
part1 _ =
  show . sumMetadata . fromRight (Node (0, 0) [] []) . parse parseNode ""

part2 :: Bool -> String -> String
part2 _ = show . value . fromRight (Node (0, 0) [] []) . parse parseNode ""
