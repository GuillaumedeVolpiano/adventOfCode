module Day13
  ( part1
  , part2
  ) where

import           Control.Applicative        (empty)
import           Data.ByteString            (ByteString)
import           Data.Either                (fromRight)
import           Data.Maybe                 (mapMaybe)
import           Data.Ratio                 (Ratio, denominator, numerator, (%))
import           Data.Word8                 (_colon, _comma, _equal, _plus,
                                             _quotesingle, _space, isAlpha)
import           Helpers.Graph              (Pos, origin)
import           Helpers.Parsers.ByteString (Parser)
import           Helpers.Search             (dijkstraGoalValSafe)
import           Linear.Matrix              (det22, inv22, transpose, (!*))
import           Linear.V1                  (V1 (..))
import           Linear.V2                  (V2 (..))
import           Text.Megaparsec            (eof, manyTill, optional, parse,
                                             takeWhileP)
import           Text.Megaparsec.Byte       (eol)
import qualified Text.Megaparsec.Byte.Lexer as L (decimal, lexeme, space)

data ClawMachine =
  ClawMachine A B Prize
  deriving (Show, Eq)

type A = V2 (Ratio Int)

type B = V2 (Ratio Int)

type Prize = V2 (Ratio Int)

space :: Parser ()
space = do
  _ <-
    takeWhileP
      Nothing
      (\c ->
         isAlpha c
           || c `elem` [_space, _quotesingle, _colon, _plus, _comma, _equal])
  return ()

parseTwo :: Parser (V2 (Ratio Int))
parseTwo = do
  space
  x <- L.decimal
  space
  y <- L.decimal
  eol
  return (V2 x y)

parseClawMachine :: Parser ClawMachine
parseClawMachine = do
  a <- parseTwo
  b <- parseTwo
  prize <- parseTwo
  optional eol
  return $ ClawMachine a b prize

parseInput :: Parser [ClawMachine]
parseInput = manyTill parseClawMachine eof

getPrize :: ClawMachine -> Maybe Int
getPrize (ClawMachine a b prize)
  | det22 matrix == 0 || denominator na /= 1 || denominator nb /= 1 = Nothing
  | otherwise = Just $ 3 * numerator na + numerator nb
  where
    matrix = V2 a b
    (V2 na nb) = (inv22 . transpose $ matrix) !* prize

farAwayPrize :: ClawMachine -> ClawMachine
farAwayPrize (ClawMachine a b (V2 xp yp)) =
  ClawMachine a b $ V2 (xp + 10 ^ 13) (yp + 10 ^ 13)

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . sum
    . mapMaybe getPrize
    . fromRight (error "could not parse input")
    . parse parseInput "day13"

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . sum
    . mapMaybe (getPrize . farAwayPrize)
    . fromRight (error "could not parse input")
    . parse parseInput "day13"
