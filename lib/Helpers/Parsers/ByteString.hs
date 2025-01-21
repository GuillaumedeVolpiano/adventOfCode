{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TupleSections    #-}

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
  , digitToInt
  , lines
  , signedInt
  , signedDouble
  , signedInts
  , signedDoubles
  , make2DArray
  , numsAsByteStrings
  , parseByLine
  , parseInput
  , splitOnSpace
  , decimal
  , double
  , string
  ) where

import           Control.Applicative       as A (empty)
import           Control.Monad             (void)
import           Data.Array.IArray         (IArray)
import           Data.Array.Unboxed        (UArray, array, (!))
import           Data.ByteString           (ByteString, cons, pack, split,
                                            unpack)
import qualified Data.ByteString           as BS (concat, empty, length)
import qualified Data.ByteString.Char8     as BSC (foldr, pack)
import           Data.ByteString.UTF8      (fromString)
import           Data.Char                 (isPrint)
import qualified Data.Char                 as C (digitToInt)
import           Data.Either               (fromRight)
import           Data.List                 as L (length)
import           Data.Maybe                (Maybe (Just, Nothing), catMaybes)
import           Data.Void                 (Void)
import           Data.Word                 (Word8)
import           Data.Word8                (_0, _1, _2, _3, _4, _5, _6, _7, _8,
                                            _9, _hyphen, _period, _space,
                                            isAlpha, isAlphaNum, isSpace)
import           FlatParse.Basic           (anyAsciiDecimalInt, byteString,
                                            char, eof, failed, isDigit,
                                            isLatinLetter, many, optional_,
                                            runParser, satisfy, skipSatisfy,
                                            switch, (<|>))
import           Helpers.Parsers.FlatParse (Parser, extract)
import           Linear.V2                 (V2 (..))
import           Prelude                   hiding (lines)

type Pos = V2 Int

digitArray =
  array
    (_0, _9)
    [ (_0, 0)
    , (_1, 1)
    , (_2, 2)
    , (_3, 3)
    , (_4, 4)
    , (_5, 5)
    , (_6, 6)
    , (_7, 7)
    , (_8, 8)
    , (_9, 9)
    ] :: UArray Word8 Int

eol :: Parser ()
eol = $(char '\n')

lines :: ByteString -> [ByteString]
lines = init . split 10

-- the supplied parser must consume all the line, including the new line
-- character
parseByLine :: Parser a -> ByteString -> [a]
parseByLine parser = extract . runParser (many parser <* eof)

parseLineList :: Parser ByteString -> ByteString -> [ByteString]
parseLineList parser =
  extract
    . runParser
        (optional_ consume
           >> many (parser >>= \x -> optional_ consume >> pure x) <* eof)

parseList :: Parser a -> ByteString -> [[a]]
parseList parser = extract . runParser (many (parseLine parser) <* eof)

parseInput :: Parser a -> Parser [[a]]
parseInput parser = many (parseLine parser) <* eof

parseLine :: Parser a -> Parser [a]
parseLine parser =
  many (consumingParser parser) <* (optional_ (many consume) >> eol)

consumingParser :: Parser a -> Parser a
consumingParser parser = parser <|> (consume >> consumingParser parser)

consume :: Parser ()
consume = skipSatisfy isPrint

digits :: Parser ByteString
digits = do
  BSC.pack <$> many (satisfy isDigit)

digitToInt :: Word8 -> Int
digitToInt = (!) digitArray

signedInt :: Parser Int
signedInt =
  $(switch
      [|case _ of
          "-" -> spaceConsumer >> anyAsciiDecimalInt >>= \x -> pure . negate $ x
          _   -> spaceConsumer >> anyAsciiDecimalInt|])

signedDouble :: Parser Double
signedDouble =
  signedInt >>= \x -> postDecimal >>= \y -> pure $ fromIntegral x + decimaled y

decimaled :: Int -> Double
decimaled x
  | x == 0 = 0
  | otherwise = doubleX / decimator
  where
    doubleX = fromIntegral x :: Double
    logged = ceiling . logBase 10 $ doubleX
    decimator = 10 ^ logged

postDecimal :: Parser Int
postDecimal = ($(char '.') >> anyAsciiDecimalInt) <|> pure 0

signedInts :: ByteString -> [[Int]]
signedInts = parseList signedInt

signedDoubles :: ByteString -> [[Double]]
signedDoubles = parseList signedDouble

numsAsByteStrings :: Parser ByteString
numsAsByteStrings = do
  s <-
    $(switch
        [|case _ of
            "-" -> pure ('-' :)
            _   -> pure id|])
  i <- ((++) <$> many (satisfy isDigit)) <|> pure id
  d <- ($(char '.') >> many (satisfy isDigit)) <|> pure []
  let result = s . i $ d
  if null result
    then failed
    else pure (fromString result)

alpha :: Parser ByteString
alpha = do
  fromString <$> many (satisfy isLatinLetter)

alnum :: Parser ByteString
alnum = fromString <$> many (satisfy (\x -> isLatinLetter x || isDigit x))

notSpace :: Parser ByteString
notSpace = fromString <$> many (satisfy (/= ' '))

alphaNum :: ByteString -> [[ByteString]]
alphaNum = parseList alnum

characters :: ByteString -> [[ByteString]]
characters = parseList alpha

custom :: Parser ByteString -> ByteString -> [[ByteString]]
custom = parseList

-- | The 'complex parser' function parses a string with complex patterns. It
-- takes as arguments a list of splitter patterns, a list of parsing patterns
-- and a string, potentially unlined, and return a list of list of lists of
-- strings, taken from the original string, split around the splitters and
-- parsed with the patterns.
complexParser ::
     [String] -> [Parser ByteString] -> ByteString -> [[[ByteString]]]
complexParser splitters pats =
  map (zipWith parseLineList pats . splitOnSplitters splitters) . lines

make2DArray :: IArray UArray a => [[a]] -> UArray (V2 Int) a
make2DArray l =
  array
    (V2 0 0, V2 width height)
    [(V2 x y, l !! y !! x) | x <- [0 .. width], y <- [0 .. height]]
  where
    width = L.length (head l) - 1
    height = L.length l - 1

arrayFromByteString :: ByteString -> UArray Pos Word8
arrayFromByteString = make2DArray . map unpack . lines

boolArrayFromByteString :: Char -> ByteString -> UArray Pos Bool
boolArrayFromByteString test =
  make2DArray . map (BSC.foldr ((:) . (== test)) []) . lines

digitArrayFromByteString :: ByteString -> UArray Pos Int
digitArrayFromByteString =
  make2DArray . map (BSC.foldr ((:) . C.digitToInt) []) . lines

splitOnSpace :: ByteString -> [[ByteString]]
splitOnSpace = parseList notSpace

splitter :: String -> Parser (ByteString, ByteString)
splitter s = do
  b <- fromString <$> (many (satisfy isPrint) <* (byteString . fromString $ s))
  (b, ) . fromString <$> (many (satisfy isPrint) <* eof)

splitOnSplitters :: [String] -> ByteString -> [ByteString]
splitOnSplitters [] aByteString = [aByteString]
splitOnSplitters (s:ss) aByteString = before : splitOnSplitters ss after
  where
    (before, after) = extract . runParser (splitter s) $ aByteString

spaceConsumer :: Parser ()
spaceConsumer = void . many $ $(char ' ')

decimal :: Parser Int
decimal = anyAsciiDecimalInt >>= \x -> many spaceConsumer >> pure x

double :: Parser Double
double =
  anyAsciiDecimalInt >>= \x ->
    (($(char '.') >> anyAsciiDecimalInt) <|> pure 0) >>= \y ->
      pure (fromIntegral x + decimaled y)

string :: String -> Parser ByteString
string = pure . fromString
