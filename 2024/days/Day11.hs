module Day11
  ( part1
  , part2
  ) where

import           Data.ByteString            (ByteString)
import           Data.Either                (fromRight)
import           Data.MultiSet              as MS (MultiSet, distinctElems,
                                                   distinctSize, empty,
                                                   fromList, insertMany, occur,
                                                   size)
import           Helpers.Parsers.ByteString (Parser, decimal)
import           Text.Megaparsec            (eof, manyTill, parse)
import           Text.Megaparsec.Byte       (eol)

type Stones = MultiSet Int

parseInput :: Parser Stones
parseInput = fromList <$> manyTill decimal eol

blink :: Stones -> Stones
blink stones = foldr (collatz stones) empty . distinctElems $ stones

collatz :: Stones -> Int -> Stones -> Stones
collatz stones stone stones'
  | stone == 0 = insertMany 1 count stones'
  | even splitter = foldr (`insertMany` count) stones' split
  | otherwise = insertMany (2024 * stone) count stones'
  where
    count = occur stone stones
    splitter = ceiling . logBase 10 . fromIntegral $ (stone + 1)
    split = [stone `mod` 10 ^ div splitter 2, stone `div` 10 ^ div splitter 2]

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . size
    . (!! 25)
    . iterate blink
    . fromRight (error "input could not be parsed")
    . parse parseInput "day11"

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . size
    . (!! 75)
    . iterate blink
    . fromRight (error "input could not be parsed")
    . parse parseInput "day11"
