module Day11
  ( part1
  , part2
  ) where

import           Data.Either          (fromRight)
import           Helpers.Parsers      (Parser)
import           Linear.V2            (V2 (..))
import           Text.Megaparsec      (optional, parse, try, (<|>))
import           Text.Megaparsec.Char (char, eol, string)

type Pos = V2 Int

parseInput :: [Pos] -> Parser [Pos]
parseInput pos = do
  dir pos <|> end pos

dir :: [Pos] -> Parser [Pos]
dir pos@(p:os) = do
  x <-
    op
      <$> (string "ne"
             <|> string "nw"
             <|> string "n"
             <|> string "se"
             <|> string "sw"
             <|> string "s")
  optional . char $ ','
  parseInput (x p : pos)

op :: String -> (Pos -> Pos)
op "ne" = (+ V2 1 (-1))
op "nw" = (+ V2 (-1) (-1))
op "n"  = (+ V2 0 (-1))
op "se" = (+ V2 1 1)
op "sw" = (+ V2 (-1) 1)
op "s"  = (+ V2 0 1)

end :: [Pos] -> Parser [Pos]
end pos = do
  eol
  return pos

dist :: Pos -> Int
dist (V2 x y)
  | x > y = x
  | otherwise = y

part1 :: Bool -> String -> String
part1 _ = show . dist . head . fromRight [] . parse (parseInput [V2 0 0]) ""

part2 :: Bool -> String -> String
part2 _ =
  show . maximum . map dist . fromRight [] . parse (parseInput [V2 0 0]) ""
