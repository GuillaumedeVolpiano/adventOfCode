module Day4
  ( part1
  , part2
  ) where

import           Data.Char            (chr, isAlpha, isDigit, ord)
import           Data.List            (group, intersperse, isInfixOf, sort,
                                       sortBy)
import           Data.Maybe           (catMaybes)
import           Data.Ord             (Down, comparing)
import           Helpers.Parsers      (Parser, parseByLine)
import           Text.Megaparsec      (many, optional, takeWhileP)
import           Text.Megaparsec.Char (char, eol)

type Cipher = ([String], Int)

parseInput :: String -> [Cipher]
parseInput = catMaybes . parseByLine parseCipher

parseCipher :: Parser (Maybe Cipher)
parseCipher = do
  name <- many word
  iD <- read <$> takeWhileP Nothing isDigit
  char '['
  checksum <- takeWhileP Nothing isAlpha
  char ']'
  optional eol
  let checked =
        take 5
          . map head
          . sortBy (flip (comparing length))
          . group
          . sort
          . concat
          $ name
      result
        | checked == checksum = Just (name, iD)
        | otherwise = Nothing
  return result

word :: Parser String
word = do
  result <- takeWhileP Nothing isAlpha
  char '-'
  return result

decipher :: Cipher -> (String, Int)
decipher (wordList, iD) = (unwords . map (map decode) $ wordList, iD)
  where
    decode c = chr . (97 +) . mod (ord c - 97 + iD) $ 26

part1 :: Bool -> String -> String
part1 _ = show . sum . map snd . parseInput

part2 :: Bool -> String -> String
part2 _ =
  show
    . snd
    . head
    . filter (\x -> "northpole" `isInfixOf` fst x)
    . map decipher
    . parseInput
