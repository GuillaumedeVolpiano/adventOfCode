
module Day6
  ( part1
  , part2
  , getWrongWorksheet
  , getTotal
  , getCorrectWorksheet
  , buildWrongWorksheet
  , getCorrectTotal
  ) where

import           Control.DeepSeq          (NFData, rnf)
import           Data.Bifunctor           (second)
import           Data.Either              (fromRight)
import qualified Data.List                as L (transpose)
import           Data.Word                (Word8)
import           Helpers.General.Streamly (digit, isDigit)
import qualified Streamly.Data.Fold       as F (drain, foldl', toList)
import           Streamly.Data.Fold       (Fold)
import qualified Streamly.Data.Parser     as P (eof, many, manyTill, satisfy,
                                                some)
import           Streamly.Data.Parser     (Parser)
import qualified Streamly.Data.Stream     as S (parse)
import           Streamly.Data.Stream     (Stream)

data Op = Add Int | Mult Int deriving (Show, Eq)

instance NFData Op where
  rnf (Add v)  = rnf v
  rnf (Mult v) = rnf v

toInt :: Monad m => Fold m Word8 Int
toInt = F.foldl' (\a b -> digit b + 10 * a) 0

correctParser :: Monad m => Parser Word8 m ([Op], [[Int]])
correctParser = P.many parseIntLineRight F.toList >>= \ints -> parseOps >>= \ops -> pure (ops, ints)

parseIntLineRight :: Monad m => Parser Word8 m [Int]
parseIntLineRight = P.manyTill ((\w -> if w == 32 then -1 else digit w) <$> P.satisfy (\w -> w == 32 || isDigit w)) (P.satisfy (== 10)) F.toList

wrongParser :: Monad m => Parser Word8 m ([Op], [[Int]])
wrongParser = P.many parseIntLineWrong F.toList >>= \ints -> parseOps >>= \ops -> pure (ops, ints)

parseIntLineWrong :: Monad m => Parser Word8 m [Int]
parseIntLineWrong = P.manyTill parseIntWrong (P.many (P.satisfy (== 32)) F.drain >> P.satisfy (==10)) F.toList

parseIntWrong :: Monad m => Parser Word8 m Int
parseIntWrong = P.many (P.satisfy (== 32)) F.drain >> P.some (P.satisfy isDigit) toInt

parseOps :: Monad m => Parser Word8 m [Op]
parseOps = do
  ops <- P.some parseOp F.toList
  P.eof
  pure ops

parseOp :: Monad m => Parser Word8  m Op
parseOp = do
  v <- P.satisfy (\w -> w == 42 || w == 43)
  l <- length <$> P.many (P.satisfy (\w -> w == 32 || w == 10)) F.toList
  pure $ if v == 42 then Mult l else Add l

getWrongWorksheet :: Stream IO Word8 -> IO ([Op], [[Int]])
getWrongWorksheet s = fromRight undefined <$> S.parse wrongParser s

getCorrectWorksheet :: Stream IO Word8 -> IO ([Op], [[Int]])
getCorrectWorksheet s = fromRight undefined <$> S.parse correctParser s

buildWrongWorksheet :: [[Int]] -> [[Int]]
buildWrongWorksheet = L.transpose

getTotal :: [Op] -> [[Int]] -> Int
getTotal os = sum  . zipWith calc os
  where
    calc (Add _)  = sum
    calc (Mult _) = product

getCorrectTotal :: [Op] -> [[Int]] -> Int
getCorrectTotal [] is
  | all null is = 0
  | otherwise = undefined
getCorrectTotal (o:os) is = r + getCorrectTotal os is'
  where
    l = case o of
          Add v  -> v
          Mult v -> v
    sis = map (splitAt l) is
    is' = map (drop 1 .snd) sis
    i = buildVals . map fst $ sis
    r = case o of
          Add _  -> sum i
          Mult _ -> product i
    buildVals = map (foldl' (\c v -> if v == -1 then c else v + 10*c) 0) . L.transpose

part1 :: Bool -> Stream IO Word8 -> IO ()
part1 _ s = getWrongWorksheet s >>= print . uncurry getTotal . second buildWrongWorksheet

part2 :: Bool -> Stream IO Word8 -> IO ()
part2 _ s = getCorrectWorksheet s >>= print . uncurry getCorrectTotal
