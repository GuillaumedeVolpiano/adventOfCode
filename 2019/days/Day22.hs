{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Day22
  ( part1
  , part2
  ) where

import           Control.Monad                  (void)
import           Data.Char                      (isDigit)
import           Data.Either                    (fromRight)
import           Data.Maybe                     (fromJust)
import           Data.Semigroup                 (stimes)
import           Helpers.Parsers                (Parser)
import           Math.NumberTheory.Moduli.Class (KnownNat, Mod, getVal,
                                                 invertMod)
import           Text.Megaparsec                (eof, many, optional, parse,
                                                 takeWhile1P, try, (<|>))
import           Text.Megaparsec.Char           (eol, string)

data Shuffle n =
  Shuffle (Mod n) (Mod n)
  deriving (Show)

instance KnownNat n => Semigroup (Shuffle n) where
  (Shuffle a b) <> (Shuffle c d) = Shuffle (a * c) (a * d + b)

instance KnownNat n => Monoid (Shuffle n) where
  mempty = Shuffle 1 0

invert :: KnownNat n => Shuffle n -> Shuffle n
invert (Shuffle x y) = Shuffle invX invY
  where
    invX = fromJust . invertMod $ x
    invY = negate (invX * y)

parser :: KnownNat n => Parser [Shuffle n]
parser = many parseLine <* eof

parseLine :: KnownNat n => Parser (Shuffle n)
parseLine = try dealNew <|> try cutN <|> incrementN
  where
    dealNew = do
      void . string $ "deal into new stack"
      void . optional $ eol
      return $ Shuffle (-1) (-1)
    cutN = do
      void . string $ "cut "
      val <-
        read <$> takeWhile1P Nothing (\x -> x == '-' || isDigit x) :: Parser Int
      void . optional $ eol
      return $ Shuffle 1 (negate . fromIntegral $ val)
    incrementN = do
      void . string $ "deal with increment "
      val <- read <$> takeWhile1P Nothing isDigit :: Parser Int
      let natVal = fromIntegral val
      void . optional $ eol
      return $ Shuffle (fromIntegral val) 0

shuffleCard :: (KnownNat n) => Shuffle n -> Mod n -> Mod n
shuffleCard (Shuffle a b) x = a * x + b

part1Test :: String -> String
part1Test input = show . getVal . shuffleCard shuffle $ 9
  where
    shuffle =
      mconcat . reverse . fromRight [] . parse parser "" $ input :: Shuffle 10

part1Full :: String -> String
part1Full input = show . getVal . shuffleCard shuffle $ 2019
  where
    shuffle =
      mconcat . reverse . fromRight [] . parse parser "" $ input :: Shuffle 10007

part1 :: Bool -> String -> String
part1 test
  | test = part1Test
  | otherwise = part1Full

part2 :: Bool -> String -> String
part2 _ input = show . getVal . shuffleCard (invert multShuffle) $ 2020
  where
    shuffle =
      mconcat . reverse . fromRight [] . parse parser "" $ input :: Shuffle 119315717514047
    multShuffle = stimes 101741582076661 shuffle
