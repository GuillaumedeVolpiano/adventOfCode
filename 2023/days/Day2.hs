{-# LANGUAGE TemplateHaskell #-}

module Day2
  ( part1
  , part2
  ) where

import           Data.Bifunctor            (second)
import           Data.ByteString           (ByteString)
import           Data.ByteString.Char8     (unpack)
import           Data.Text                 (Text, pack)
import           FlatParse.Basic           (anyAsciiDecimalInt, char, optional_,
                                            runParser, some, string, switch,
                                            (<|>))
import           Helpers.Parsers.FlatParse (Parser, extract)
import           Helpers.Parsers.Text      (characters, signedInts)

type Game = (Int, Round)

type Round = [(Int, Colour)]

data Colour
  = Blue
  | Red
  | Green
  deriving (Eq)

parseInput :: Parser [Game]
parseInput = some parseLine

parseLine :: Parser Game
parseLine = do
  $(string "Game ")
  iD <- anyAsciiDecimalInt
  $(string ": ")
  pairs <- some parsePair
  $(char '\n')
  pure (iD, pairs)

parsePair :: Parser (Int, Colour)
parsePair = do
  count <- anyAsciiDecimalInt
  $(char ' ')
  colour <-
    $(switch
        [|case _ of
            "blue"  -> pure Blue
            "red"   -> pure Red
            "green" -> pure Green|])
  optional_ (($(char ',') <|> $(char ';')) >> $(char ' '))
  pure (count, colour)

countPossible :: [Game] -> Int
countPossible = sum . map fst . filter (all validPair . snd)
  where
    validPair (b, Blue)  = b <= 14
    validPair (g, Green) = g <= 13
    validPair (r, Red)   = r <= 12

power :: Game -> Int
power (_, round) = red * green * blue
  where
    red = only Red
    green = only Green
    blue = only Blue
    only c = maximum . map fst . filter (\p -> snd p == c) $ round

part1 :: Bool -> ByteString -> String
part1 _ = show . countPossible . extract . runParser parseInput

part2 :: Bool -> ByteString -> String
part2 _ = show . sum . map power . extract . runParser parseInput
