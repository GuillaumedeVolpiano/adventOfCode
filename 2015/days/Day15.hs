{-# LANGUAGE TemplateHaskell #-}

module Day15
  ( part1
  , part2
  ) where

import           Control.Monad             (void)
import           Data.ByteString           (ByteString)
import           Data.List                 (transpose)
import           FlatParse.Basic           (anyAsciiDecimalInt, eof, isDigit,
                                            many, runParser, skipSatisfy,
                                            switch, (<|>))
import           Helpers.Parsers.FlatParse (extract)
import qualified Helpers.Parsers.FlatParse as F (Parser)

proportions :: [Int]
proportions = [1 .. 97]

use :: [[Int]] -> [Int] -> Int
use ingredients props =
  product . map (max 0 . sum . zipWith (*) props) . init $ ingredients

consumer :: F.Parser ()
consumer = void . many $ skipSatisfy (\x -> x /= '-' && not (isDigit x))

number :: F.Parser Int
number = do
  consumer
  x <-
    $(switch
        [|case _ of
            "-" -> negate <$> anyAsciiDecimalInt
            _   -> anyAsciiDecimalInt|])
  consumer
  return x

simpleParser :: F.Parser [Int]
simpleParser = do
  capacity <- number
  durability <- number
  flavor <- number
  texture <- number
  calories <- number
  return [capacity, durability, flavor, texture, calories]

simpleRecipe :: [[Int]] -> Int
simpleRecipe ingredients =
  maximum
    [ use ingredients [100 - x - y - z, x, y, z]
    | x <- proportions
    , y <- proportions
    , z <- proportions
    , x + y + z <= 99
    ]

caloriesRecipe :: [[Int]] -> Int
caloriesRecipe ingredients =
  maximum
    [ use ingredients [100 - x - y - z, x, y, z]
    | x <- proportions
    , y <- proportions
    , z <- proportions
    , x + y + z <= 99
    , calories [100 - x - y - z, x, y, z] == 500
    ]
  where
    calories = sum . zipWith (*) (last ingredients)

part1 :: Bool -> ByteString -> String
part1 _ =
  show . simpleRecipe . transpose . extract . runParser (many simpleParser)

part2 :: Bool -> ByteString -> String
part2 _ =
  show . caloriesRecipe . transpose . extract . runParser (many simpleParser)
