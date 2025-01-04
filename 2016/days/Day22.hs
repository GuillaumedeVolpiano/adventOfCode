module Day22
  ( part1
  , part2
  ) where

import           Control.Monad              (void)
import           Data.Bits                  (shiftL, shiftR)
import           Data.Char                  (isAlpha, isSymbol)
import           Data.Either                (fromRight)
import           Data.IntMap                (IntMap, elems, insert)
import qualified Data.IntMap                as M (empty)
import           Data.Text                  (Text)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (empty, eof, manyTill, parse,
                                             takeWhile1P, (<|>))
import           Text.Megaparsec.Char       (char, eol, letterChar)
import qualified Text.Megaparsec.Char.Lexer as MCL (decimal, lexeme, space)

type Disk = IntMap (Used, Available)

type Used = Int

type Available = Int

sc :: Parser ()
sc = void . takeWhile1P Nothing $ (\x -> isAlpha x || x `elem` " @-#/%")

space :: Parser ()
space = MCL.space sc empty empty

lexeme = MCL.lexeme space

decimal = lexeme MCL.decimal

parseInput :: Parser Disk
parseInput = do
  manyTill sc eol
  manyTill sc eol
  parseDisk

parseDisk :: Parser Disk
parseDisk = parsePartition <|> (eof >> return M.empty)

countPairs ::
     (Used, Available)
  -> (Int, [(Used, Available)])
  -> (Int, [(Used, Available)])
countPairs file (acc, xs) = (acc + foldr (countPair file) 0 xs, tail xs)

countPair :: (Used, Available) -> (Used, Available) -> Int -> Int
countPair (u1, a1) (u2, a2)
  | u2 /= 0 && a1 >= u2 && u1 /= 0 && a2 >= u1 = (+ 2)
  | (u2 /= 0 && a1 >= u2) || (u1 /= 0 && a2 >= u1) = (+ 1)
  | otherwise = id

findPairs :: Disk -> Int
findPairs disk = fst . foldr countPairs (0, tail xs) $ xs
  where
    xs = elems disk

parsePartition :: Parser Disk
parsePartition = do
  sc
  x <- decimal
  y <- decimal
  void decimal
  used <- decimal
  available <- decimal
  void decimal
  eol
  insert (x + shiftL y 6) (used, available) <$> parseDisk

part1 :: Bool -> Text -> String
part1 _ =
  show
    . findPairs
    . fromRight (error "parse failed")
    . parse parseInput "Day 22"

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
