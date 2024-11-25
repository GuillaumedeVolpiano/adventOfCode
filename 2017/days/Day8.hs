module Day8
  ( part1
  , part2
  ) where

import           Data.Either           (fromRight)
import           Data.Map              as M (Map, elems, empty, insert, lookup,
                                             (!?))
import           Data.Maybe            (fromMaybe)
import           Helpers.Parsers       (Parser, alpha, nums)
import           Text.Megaparsec       (eof, optional, parse, try, (<|>))
import           Text.Megaparsec.Char  (char, eol, string)

type Memory = Map Register Val

type Register = String

type Val = Int

parseCommand :: Val -> Memory -> Parser (Int, Int)
parseCommand maxVal memory = do
  Just register <- alpha
  char ' '
  op <- inc <|> dec
  char ' '
  Just v1 <- nums
  string " if "
  Just ref <- (fromMaybe 0 . (!?) memory <$>) <$> alpha
  char ' '
  comp <- try le <|> try lt <|> try ge <|> try gt <|> ne <|> eq
  char ' '
  Just v2 <- nums
  optional eol
  let curVal = fromMaybe 0 . M.lookup register $ memory
      newVal = op curVal v1
      newMem
        | comp ref v2 = insert register newVal memory
        | otherwise = memory
      newMax
        | comp ref v2 = max maxVal newVal
        | otherwise = maxVal
  end newMax newMem <|> parseCommand newMax newMem

end :: Val -> Memory -> Parser (Int, Int)
end maxVal memory = do
  eof
  return (maxVal, maximum . elems $ memory)

inc :: Parser (Int -> Int -> Int)
inc = do
  string "inc"
  return (+)

dec :: Parser (Int -> Int -> Int)
dec = do
  string "dec"
  return (-)

lt :: Parser (Int -> Int -> Bool)
lt = do
  string "<"
  return (<)

le :: Parser (Int -> Int -> Bool)
le = do
  string "<="
  return (<=)

gt :: Parser (Int -> Int -> Bool)
gt = do
  string ">"
  return (>)

ge :: Parser (Int -> Int -> Bool)
ge = do
  string ">="
  return (>=)

eq :: Parser (Int -> Int -> Bool)
eq = do
  string "=="
  return (==)

ne :: Parser (Int -> Int -> Bool)
ne = do
  string "!="
  return (/=)

part1 :: Bool -> String -> String
part1 _ = show . snd . fromRight (0, 0) . parse (parseCommand 0 empty) ""

part2 :: Bool -> String -> String
part2 _ = show . fst . fromRight (0, 0) . parse (parseCommand 0 empty) ""
