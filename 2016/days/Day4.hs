module Day4
  ( part1
  , part2
  ) where

import           Data.Char            (chr, isAlpha, isDigit, ord)
import           Data.List            as L (group, intersperse, isInfixOf, sort,
                                       sortBy, concat)
import           Data.Maybe           (catMaybes)
import           Data.Ord             (Down, comparing)
import           Data.Text            as T (Text, pack, concat, unpack)
import           Helpers.Parsers.Text (Parser, parseByLine, decimal)
import           Text.Megaparsec      (many, optional, takeWhileP)
import           Text.Megaparsec.Char (char, eol)

type Cipher = ([String], Int)

parseInput :: Text -> [Cipher]
parseInput = catMaybes . parseByLine parseCipher

parseCipher :: Parser (Maybe Cipher)
parseCipher = do
  name <- many word
  iD <- decimal
  char '['
  checksum <- unpack <$> takeWhileP Nothing isAlpha
  char ']'
  optional eol
  let checked =
        take 5
          . map head
          . sortBy (flip (comparing length))
          . group
          . sort
          . L.concat
          $ name
      result
        | checked == checksum = Just (name, iD)
        | otherwise = Nothing
  return result

word :: Parser String 
word = do
  result <- unpack <$> takeWhileP Nothing isAlpha
  char '-'
  return result

decipher :: Cipher -> (String, Int)
decipher (wordList, iD) = (unwords . map (map decode) $ wordList, iD)
  where
    decode c = chr . (97 +) . mod (ord c - 97 + iD) $ 26

part1 :: Bool -> Text -> String
part1 _ = show . sum . map snd . parseInput

part2 :: Bool -> Text -> String
part2 _ =
  show
    . snd
    . head
    . filter (\x -> "northpole" `isInfixOf` fst x)
    . map decipher
    . parseInput
