{-# LANGUAGE FlexibleContexts #-}

module Helpers.Parsers.ByteString
  ( Parser
  , alnum
  , alpha
  , alphaNum
  , arrayFromByteString
  , boolArrayFromByteString
  , characters
  , complexParser
  , custom
  , digits
  , digitArrayFromByteString
  , lines
  , signedInt
  , signedDouble
  , signedInts
  , signedDoubles
  , lexeme
  , make2DArray
  , numsAsByteStrings
  , parseByLine
  , parseInput
  , splitOnSpace
  , decimal
  , double
  , string
  ) where

import           Control.Applicative        as A (empty)
import           Data.Array.IArray          (IArray)
import           Data.Array.Unboxed         (UArray, array)
import           Data.ByteString            (ByteString, cons, pack, split)
import qualified Data.ByteString            as BS (concat, empty, length)
import           Data.ByteString.Char8      (index)
import qualified Data.ByteString.Char8      as BSC (foldr)
import           Data.ByteString.UTF8       (fromString)
import           Data.Char                  (digitToInt)
import           Data.Either                (fromRight)
import           Data.List                  as L (length)
import           Data.Maybe                 (Maybe (Just, Nothing), catMaybes)
import           Data.Void                  (Void)
import           Data.Word8                 (_hyphen, _period, _space, isAlpha,
                                             isAlphaNum, isDigit, isSpace)
import           Linear.V2                  (V2 (..))
import           Prelude                    hiding (lines)
import           Text.Megaparsec            (Parsec, eof, manyTill, optional,
                                             parse, someTill, takeWhile1P,
                                             takeWhileP, try, (<|>))
import qualified Text.Megaparsec.Byte       as MB (string)
import           Text.Megaparsec.Byte       (char, eol, printChar, space1)
import qualified Text.Megaparsec.Byte.Lexer as L (decimal, float, lexeme,
                                                  signed, skipBlockComment,
                                                  skipLineComment, space,
                                                  symbol)

type Parser = Parsec Void ByteString

type Pos = V2 Int

lines :: ByteString -> [ByteString]
lines = split 10

-- the supplied parser must consume all the line, including the new line
-- character
parseByLine :: Parser a -> ByteString -> [a]
parseByLine parser = fromRight [] . parse (manyTill parser eof) ""

parseLineList :: Parser (Maybe ByteString) -> ByteString -> [ByteString]
parseLineList parser =
  fromRight [] . parse (catMaybes <$> manyTill (try parser <|> consume) eof) ""

parseList :: Parser (Maybe a) -> ByteString -> [[a]]
parseList parser = fromRight [] . parse (manyTill (parseLine parser) eof) ""

parseInput :: Parser (Maybe a) -> Parser [[a]]
parseInput parser = manyTill (parseLine parser) eof

parseLine :: Parser (Maybe a) -> Parser [a]
parseLine parser = catMaybes <$> manyTill (try parser <|> consume) eol

consume :: Parser (Maybe a)
consume = do
  _ <- printChar
  return Nothing

digits :: Parser (Maybe ByteString)
digits = do
  Just <$> takeWhile1P Nothing isDigit

signedInt :: Parser (Maybe Int)
signedInt = Just <$> L.signed spaceConsumer decimal

signedDouble :: Parser (Maybe Double)
signedDouble = Just <$> L.signed spaceConsumer double

signedInts :: ByteString -> [[Int]]
signedInts = parseList signedInt

signedDoubles :: ByteString -> [[Double]]
signedDoubles = parseList signedDouble

numsAsByteStrings :: Parser (Maybe ByteString)
numsAsByteStrings = do
  s <-
    optional . try $ do
      char _hyphen
  i <- takeWhile1P Nothing isDigit
  d <-
    optional . try $ do
      sep <- char _period
      dec <- takeWhile1P Nothing isDigit
      return (cons sep dec)
  return (Just . BS.concat . catMaybes $ [fmap (`cons` BS.empty) s, Just i, d])

alpha :: Parser (Maybe ByteString)
alpha = do
  Just <$> takeWhile1P Nothing isAlpha

alnum :: Parser (Maybe ByteString)
alnum = do
  Just <$> takeWhile1P Nothing isAlphaNum

notSpace :: Parser (Maybe ByteString)
notSpace = do
  Just <$> takeWhileP Nothing (not . isSpace)

alphaNum :: ByteString -> [[ByteString]]
alphaNum = parseList alnum

characters :: ByteString -> [[ByteString]]
characters = parseList alpha

custom :: Parser (Maybe ByteString) -> ByteString -> [[ByteString]]
custom = parseList

-- | The 'complex parser' function parses a string with complex patterns. It
-- takes as arguments a list of splitter patterns, a list of parsing patterns
-- and a string, potentially unlined, and return a list of list of lists of
-- strings, taken from the original string, split around the splitters and
-- parsed with the patterns.
complexParser ::
     [String] -> [Parser (Maybe ByteString)] -> ByteString -> [[[ByteString]]]
complexParser splitters pats =
  map (zipWith parseLineList pats . splitOnSplitters splitters) . lines

make2DByteStringArray :: [ByteString] -> UArray (V2 Int) Char
make2DByteStringArray l =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, l !! y `index` x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = BS.length (head l) - 1
    height = length l - 1

make2DArray :: IArray UArray a => [[a]] -> UArray (V2 Int) a
make2DArray l =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, l !! y !! x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = L.length (head l) - 1
    height = L.length l - 1

arrayFromByteString :: ByteString -> UArray Pos Char
arrayFromByteString = make2DByteStringArray . lines

boolArrayFromByteString :: Char -> ByteString -> UArray Pos Bool
boolArrayFromByteString test =
  make2DArray . map (BSC.foldr ((:) . (== test)) []) . lines

digitArrayFromByteString :: ByteString -> UArray Pos Int
digitArrayFromByteString =
  make2DArray . map (BSC.foldr ((:) . digitToInt) []) . lines

splitOnSpace :: ByteString -> [[ByteString]]
splitOnSpace = parseList notSpace

splitter :: String -> Parser (ByteString, ByteString)
splitter s = do
  b <- pack <$> manyTill printChar (string s)
  _ <- string s
  a <- pack <$> manyTill printChar eof
  return (b, a)

splitOnSplitters :: [String] -> ByteString -> [ByteString]
splitOnSplitters [] aByteString = [aByteString]
splitOnSplitters (s:ss) aByteString = before : splitOnSplitters ss after
  where
    (before, after) =
      fromRight (BS.empty, BS.empty) . parse (splitter s) "" $ aByteString

spaceConsumer :: Parser ()
spaceConsumer =
  L.space
    (do
       char _space
       return ())
    A.empty
    A.empty

lexeme = L.lexeme spaceConsumer

decimal :: Parser Int
decimal = lexeme L.decimal

double :: Parser Double
double = lexeme L.float

string :: String -> Parser ByteString
string = MB.string . fromString
