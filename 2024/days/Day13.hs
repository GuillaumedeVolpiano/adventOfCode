module Day13
  ( part1
  , part2
  ) where

import           Control.Applicative        (empty)
import           Data.Char                  (isAlpha)
import           Data.Either                (fromRight)
import           Data.Maybe                 (mapMaybe)
import           Data.Ratio                 (denominator, numerator, (%))
import           Data.Text                  (Text)
import           Helpers.Graph              (Pos, origin)
import           Helpers.Parsers.Text       (Parser)
import           Helpers.Search             (dijkstraGoalValSafe)
import           Linear.V2                  (V2 (..))
import           Text.Megaparsec            (eof, manyTill, optional, parse,
                                             takeWhileP)
import           Text.Megaparsec.Char       (eol)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, lexeme, space)

data ClawMachine =
  ClawMachine A B Prize
  deriving (Show, Eq)

type A = V2 Int

type B = V2 Int

type Prize = V2 Int

space :: Parser ()
space = do
  _ <- takeWhileP Nothing (\c -> isAlpha c || c `elem` " ' :+,=")
  return ()

parseClawMachine :: Parser ClawMachine
parseClawMachine = do
  space
  xa <- L.decimal
  space
  ya <- L.decimal
  eol
  space
  xb <- L.decimal
  space
  yb <- L.decimal
  eol
  space
  xp <- L.decimal
  space
  yp <- L.decimal
  eol
  optional eol
  return $ ClawMachine (V2 xa ya) (V2 xb yb) (V2 xp yp)

parseInput :: Parser [ClawMachine]
parseInput = manyTill parseClawMachine eof

getPrize :: ClawMachine -> Maybe Int
getPrize (ClawMachine (V2 xa ya) (V2 xb yb) (V2 xp yp))
  | det == 0 = Nothing
  | denominator na == 1 && denominator nb == 1 =
    Just $ 3 * numerator na + numerator nb
  | otherwise = Nothing
  where
    det = xa * yb - xb * ya
    na = (yb % det) * (xp % 1) - (xb % det) * (yp % 1)
    nb = (xa % det) * (yp % 1) - (ya % det) * (xp % 1)

farAwayPrize :: ClawMachine -> ClawMachine
farAwayPrize (ClawMachine a b (V2 xp yp)) =
  ClawMachine a b $ V2 (xp + 10000000000000) (yp + 10000000000000)

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
