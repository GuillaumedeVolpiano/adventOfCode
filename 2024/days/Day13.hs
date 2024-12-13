module Day13
  ( part1
  , part2
  ) where

import           Control.Applicative        (empty)
import           Data.Char                  (isAlpha)
import           Data.Either                (fromRight)
import           Data.Maybe                 (mapMaybe)
import           Data.Ratio                 (Ratio, denominator, numerator, (%))
import           Data.Text                  (Text)
import           Helpers.Graph              (Pos, origin)
import           Helpers.Parsers.Text       (Parser)
import           Helpers.Search             (dijkstraGoalValSafe)
import           Linear.Matrix              (det22, inv22, transpose, (!*))
import           Linear.V1                  (V1 (..))
import           Linear.V2                  (V2 (..))
import           Text.Megaparsec            (eof, manyTill, optional, parse,
                                             takeWhileP)
import           Text.Megaparsec.Char       (eol)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space)

data ClawMachine =
  ClawMachine A B Prize
  deriving (Show, Eq)

type A = V2 (Ratio Int)

type B = V2 (Ratio Int)

type Prize = V2 (Ratio Int)

space :: Parser ()
space = do
  _ <- takeWhileP Nothing (\c -> isAlpha c || c `elem` " ' :+,=")
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

part1 :: Bool -> Text -> String
part1 _ =
  show
    . sum
    . mapMaybe getPrize
    . fromRight (error "could not parse input")
    . parse parseInput "day13"

part2 :: Bool -> Text -> String
part2 _ =
  show
    . sum
    . mapMaybe (getPrize . farAwayPrize)
    . fromRight (error "could not parse input")
    . parse parseInput "day13"
