{-# LANGUAGE FlexibleContexts #-}

module Helpers.Parsers
  ( Parser
  , alnum
  , alpha
  , alphaNum
  , arrayFromText
  , boolArrayFromText
  , characters
  , complexParser
  , custom
  , digits
  , digitArrayFromText
  , signedInt
  , signedDouble
  , lexeme
  , make2DArray
  , numsAsTexts
  , parseByLine
  , parseInput
  , splitOnSpace
  , decimal
  , double
  ) where

import           Control.Applicative        as A (empty)
import           Data.Array.IArray          (IArray)
import           Data.Array.Unboxed         (UArray, array)
import           Data.Char                  (digitToInt, isAlpha, isAlphaNum,
                                             isDigit, isSpace)
import           Data.Either                (fromRight)
import Data.List as L (length)
import           Data.Maybe                 (Maybe (Just, Nothing), catMaybes)
import           Data.Text                  as T (Text, concat, cons, empty, lines, index, length, foldr, pack)
import           Data.Void                  (Void)
import           Linear.V2                  (V2 (..))
import           Text.Megaparsec            (Parsec, eof, manyTill, optional,
                                             parse, someTill, takeWhile1P,
                                             takeWhileP, try, (<|>))
import           Text.Megaparsec.Char       (char, eol, printChar, space1,
                                             string)
import qualified Text.Megaparsec.Char.Lexer as L (decimal, float, lexeme,
                                                  signed, skipBlockComment,
                                                  skipLineComment, space)

type Parser = Parsec Void Text

type Pos = V2 Int

-- the supplied parser must consume all the line, including the new line
-- character
parseByLine :: Parser a -> Text -> [a]
parseByLine parser = fromRight [] . parse (manyTill parser eof) ""

parseLineList :: Parser (Maybe Text) -> Text -> [Text]
parseLineList parser =
  fromRight [] . parse (catMaybes <$> manyTill (try parser <|> consume) eof) ""

parseList :: Parser (Maybe a) -> Text -> [[a]]
parseList parser = fromRight [] . parse (manyTill (parseLine parser) eof) ""

parseInput :: Parser (Maybe a) -> Parser [[a]]
parseInput parser = manyTill (parseLine parser) eof

parseLine :: Parser (Maybe a) -> Parser [a]
parseLine parser = catMaybes <$> manyTill (try parser <|> consume) eol

consume :: Parser (Maybe a)
consume = do
  _ <- printChar
  return Nothing

digits :: Parser (Maybe Text)
digits = do
  Just <$> takeWhile1P Nothing isDigit

signedInt :: Parser (Maybe Int)
signedInt = Just <$> L.signed spaceConsumer decimal

signedDouble :: Parser (Maybe Double)
signedDouble = Just <$> L.signed spaceConsumer double

numsAsTexts :: Parser (Maybe Text)
numsAsTexts = do
  s <-
    optional . try $ do
      char '-'
  i <- takeWhile1P Nothing isDigit
  d <-
    optional . try $ do
      sep <- char '.'
      dec <- takeWhile1P Nothing isDigit
      return (cons sep dec)
  return (Just . T.concat . catMaybes $ [fmap (`cons` T.empty) s, Just i, d])

alpha :: Parser (Maybe Text)
alpha = do
  Just <$> takeWhile1P Nothing isAlpha

alnum :: Parser (Maybe Text)
alnum = do
  Just <$> takeWhile1P Nothing isAlphaNum

notSpace :: Parser (Maybe Text)
notSpace = do
  Just <$> takeWhileP Nothing (not . isSpace)

alphaNum :: Text -> [[Text]]
alphaNum = parseList alnum

characters :: Text -> [[Text]]
characters = parseList alpha

custom :: Parser (Maybe Text) -> Text -> [[Text]]
custom = parseList

-- | The 'complex parser' function parses a string with complex patterns. It
-- takes as arguments a list of splitter patterns, a list of parsing patterns
-- and a string, potentially unlined, and return a list of list of lists of
-- strings, taken from the original string, split around the splitters and
-- parsed with the patterns.
complexParser :: [Text] -> [Parser (Maybe Text)] -> Text -> [[[Text]]]
complexParser splitters pats =
  map (zipWith parseLineList pats . splitOnSplitters splitters) . T.lines

make2DTextArray :: [Text] -> UArray (V2 Int) Char 
make2DTextArray l =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, l !! y `index` x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = T.length (head l) - 1
    height = L.length l - 1

make2DArray :: IArray UArray a => [[a]] -> UArray (V2 Int) a
make2DArray l =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, l !! y !! x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = L.length (head l) - 1
    height = L.length l - 1

arrayFromText :: Text -> UArray Pos Char
arrayFromText = make2DTextArray . T.lines

boolArrayFromText :: Char -> Text -> UArray Pos Bool
boolArrayFromText test = make2DArray . map (T.foldr ((:) . (== test)) []) . T.lines

digitArrayFromText :: Text -> UArray Pos Int
digitArrayFromText = make2DArray . map (T.foldr ((:) . digitToInt) []) . T.lines

splitOnSpace :: Text -> [[Text]]
splitOnSpace = parseList notSpace

splitter :: Text -> Parser (Text, Text)
splitter s = do
  b <- pack <$> manyTill printChar (string s)
  _ <- string s
  a <- pack <$> manyTill printChar eof
  return (b, a)

splitOnSplitters :: [Text] -> Text -> [Text]
splitOnSplitters [] aText = [aText]
splitOnSplitters (s:ss) aText = before : splitOnSplitters ss after
  where
    (before, after) = fromRight (T.empty, T.empty) . parse (splitter s) "" $ aText

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    (do
       char ' '
       return ())
    A.empty
    A.empty

lexeme = L.lexeme spaceConsumer

decimal :: Parser Int
decimal = lexeme L.decimal

double :: Parser Double
double = lexeme L.float
