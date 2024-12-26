module Day22
  ( part1
  , part2
  ) where

import           Data.IntMap                (IntMap)
import           Data.Text                  (Text)
import           Helpers.Parsers.Text       (Parser)
import           Text.Megaparsec            (eof, parse, manyTill, (<|>))
import           Text.Megaparsec.Char       (letterChar, char, eol)
import           Text.Megaparsec.Char.Lexer (decimal, lexeme, space)
import Control.Monad (void)

type Disk = IntMap (Available, Free)

type Available = Int

type Free = Int

part1 :: Bool -> Text -> String
part1 _ _ = "Part 1"

part2 :: Bool -> Text -> String
part2 _ _ = "Part 2"
