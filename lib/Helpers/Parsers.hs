{-# LANGUAGE FlexibleContexts #-}

module Helpers.Parsers
  ( Parser
  , Pos
  , alnum
  , alpha
  , alphaNum
  , arrayFromString
  , boolArrayFromString
  , characters
  , complexParser
  , custom
  , digits
  , doubles
  , digitArrayFromString
  , integers
  , make2DArray
  , numbers
  , nums
  , numsAsStrings
  , parseByLine
  , splitOnSpace
  ) where

import           Data.Array.IArray    (IArray)
import           Data.Array.Unboxed   (UArray, array)
import           Data.Char            (digitToInt, isAlpha, isAlphaNum, isDigit,
                                       isSpace)
import           Data.Either          (fromRight)
import           Data.Maybe           (Maybe (Just, Nothing), catMaybes)
import           Data.Void            (Void)
import           Linear.V2            (V2 (..))
import           Text.Megaparsec      (Parsec, eof, manyTill, optional, parse,
                                       someTill, takeWhile1P, takeWhileP, try,
                                       (<|>))
import           Text.Megaparsec.Char (char, eol, printChar, string)

type Parser = Parsec Void String
type Pos = V2 Int

-- the supplied parser must consume all the line, including the new line
-- character
parseByLine :: Parser a -> String -> [a]
parseByLine parser = fromRight [] . parse (manyTill parser eof) ""

parseLineList :: Parser (Maybe String) -> String -> [String]
parseLineList parser =
  fromRight [] . parse (catMaybes <$> manyTill (try parser <|> consume) eof) ""

parseList :: Parser (Maybe a) -> String -> [[a]]
parseList parser = fromRight [] . parse (manyTill (parseLine parser) eof) ""

parseInput :: Parser (Maybe a) -> Parser [[a]]
parseInput parser = manyTill (parseLine parser) eof

parseLine :: Parser (Maybe a) -> Parser [a]
parseLine parser = catMaybes <$> manyTill (try parser <|> consume) eol

consume :: Parser (Maybe a)
consume = do
  _ <- printChar
  return Nothing

digits :: Parser (Maybe String)
digits = do
  Just <$> takeWhile1P Nothing isDigit

nums :: (Num a, Read a) => Parser (Maybe a)
nums = do
  s <- optional . try $ do char '-'
  i <- takeWhile1P Nothing isDigit
  d <-
    optional . try $ do
      sep <- char '.'
      dec <- takeWhile1P Nothing isDigit
      return (sep : dec)
  return (Just . read . concat . catMaybes $ [fmap (: []) s, Just i, d])

numsAsStrings :: Parser (Maybe String)
numsAsStrings = do
  s <- optional . try $ do char '-'
  i <- takeWhile1P Nothing isDigit
  d <-
    optional . try $ do
      sep <- char '.'
      dec <- takeWhile1P Nothing isDigit
      return (sep : dec)
  return (Just . concat . catMaybes $ [fmap (: []) s, Just i, d])

alpha :: Parser (Maybe String)
alpha = do
  Just <$> takeWhile1P Nothing isAlpha

alnum :: Parser (Maybe String)
alnum = do
  Just <$> takeWhile1P Nothing isAlphaNum

notSpace :: Parser (Maybe String)
notSpace = do
  Just <$> takeWhileP Nothing (not . isSpace)

alphaNum :: String -> [[String]]
alphaNum = parseList alnum

characters :: String -> [[String]]
characters = parseList alpha

custom :: Parser (Maybe String) -> String -> [[String]]
custom = parseList

doubles :: String -> [[Double]]
doubles = numbers

integers :: String -> [[Int]]
integers = numbers

numbers :: (Num a, Read a) => String -> [[a]]
numbers = parseList nums

-- | The 'complex parser' function parses a string with complex patterns. It
-- takes as arguments a list of splitter patterns, a list of parsing patterns
-- and a string, potentially unlined, and return a list of list of lists of
-- strings, taken from the original string, split around the splitters and
-- parsed with the patterns.
complexParser :: [String] -> [Parser (Maybe String)] -> String -> [[[String]]]
complexParser splitters pats =
  map (zipWith parseLineList pats . splitOnSplitters splitters) . lines

make2DArray :: IArray UArray a => [[a]] -> UArray (V2 Int) a
make2DArray l =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, l !! y !! x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = length (head l) - 1
    height = length l - 1

arrayFromString :: String -> UArray Pos Char
arrayFromString = make2DArray . lines

boolArrayFromString :: Char -> String -> UArray Pos Bool
boolArrayFromString test = make2DArray . map (map (== test)) . lines

digitArrayFromString :: String -> UArray Pos Int
digitArrayFromString = make2DArray . map (map digitToInt) . lines

splitOnSpace :: String -> [[String]]
splitOnSpace = parseList notSpace

splitter :: String -> Parser (String, String)
splitter s = do
  b <- manyTill printChar (string s)
  _ <- string s
  a <- manyTill printChar eof
  return (b, a)

splitOnSplitters :: [String] -> String -> [String]
splitOnSplitters [] aString = [aString]
splitOnSplitters (s:ss) aString = before : splitOnSplitters ss after
  where
    (before, after) = fromRight ("", "") . parse (splitter s) "" $ aString
