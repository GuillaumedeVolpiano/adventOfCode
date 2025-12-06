
module Day6
  ( part1
  , part2
  , getWrongWorksheet
  , getTotal
  , getCorrectWorksheet
  , buildWrongWorksheet
  , buildCorrectWorksheet
  ) where

import           Control.Monad            (void)
import           Data.Bifunctor           (second)
import           Data.Char                (chr)
import           Data.Either              (fromRight)
import qualified Data.List                as L (transpose)
import qualified Data.List.Split          as L (splitWhen)
import           Data.Word                (Word8)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (drain, foldl', toList)
import           Streamly.Data.Fold       (Fold)
import qualified Streamly.Data.Parser     as P (many, manyTill, satisfy, some)
import           Streamly.Data.Parser     (Parser)
import qualified Streamly.Data.Stream     as S (parse)
import           Streamly.Data.Stream     (Stream)
import Control.DeepSeq (NFData, rnf)

data Op = Add | Mult deriving (Show, Eq)

instance NFData Op where
  rnf Add = rnf False
  rnf Mult = rnf True

toInt :: Monad m => Fold m Word8 Int
toInt = F.foldl' (\a b -> digit b + 10 * a) 0

correctParser :: Monad m => Parser Word8 m ([Op], [String])
correctParser = P.many parseIntLineRight F.toList >>= \ints -> parseOps >>= \ops -> pure (ops, ints)

parseIntLineRight :: Monad m => Parser Word8 m String
parseIntLineRight = P.manyTill (chr . fromIntegral <$> P.satisfy (\w -> w == 32 || isDigit w)) (P.satisfy (== 10)) F.toList

wrongParser :: Monad m => Parser Word8 m ([Op], [[Int]])
wrongParser = P.many parseIntLineWrong F.toList >>= \ints -> parseOps >>= \ops -> pure (ops, ints)

parseIntLineWrong :: Monad m => Parser Word8 m [Int]
parseIntLineWrong = P.manyTill parseIntWrong (P.many (P.satisfy (== 32)) F.drain >> P.satisfy (==10)) F.toList

parseIntWrong :: Monad m => Parser Word8 m Int
parseIntWrong = P.many (P.satisfy (== 32)) F.drain >> P.some (P.satisfy isDigit) toInt

parseOps :: Monad m => Parser Word8 m [Op]
parseOps = do
  ops <- P.some parseOp F.toList
  P.many (P.satisfy (== 32)) F.drain
  void $ P.satisfy (== 10)
  pure ops

parseOp :: Monad m => Parser Word8  m Op
parseOp = do
  P.many (P.satisfy (==32)) F.drain
  v <- P.satisfy (\w -> w == 42 || w == 43)
  pure $ if v == 42 then Mult else Add

getWrongWorksheet :: Stream IO Word8 -> IO ([Op], [[Int]])
getWrongWorksheet s = fromRight undefined <$> S.parse wrongParser s

getCorrectWorksheet :: Stream IO Word8 -> IO ([Op], [String])
getCorrectWorksheet s = fromRight undefined <$> S.parse correctParser s

buildWrongWorksheet :: [[Int]] -> [[Int]]
buildWrongWorksheet = L.transpose

buildCorrectWorksheet :: [String] -> [[Int]]
buildCorrectWorksheet = map (map read) . L.splitWhen (all (== ' ')) . L.transpose

getTotal :: [Op] -> [[Int]] -> Int
getTotal os = sum  . zipWith calc os
  where
    calc Add  = sum
    calc Mult = product

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = getWrongWorksheet s >>= print . uncurry getTotal . second buildWrongWorksheet

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = getCorrectWorksheet s >>= print . uncurry getTotal . second buildCorrectWorksheet
