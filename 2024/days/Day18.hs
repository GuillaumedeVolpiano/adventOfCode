module Day18
  ( part1
  , part2
  ) where

import           Control.Monad.State        (State, evalState, get, put)
import           Data.Bits                  (shiftL, shiftR, (.&.))
import           Data.Either                (fromRight)
import           Data.IntSet                as S (IntSet, difference, empty,
                                                  fromList, intersection,
                                                  member, notMember, null)
import           Data.List                  (inits)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (Text, unpack)
import           Helpers.Parsers.Text       (Parser)
import           Helpers.Search.Int         (bfsSafe, bfsSafeDist, dfs)
import           Text.Megaparsec            (eof, manyTill, parse)
import           Text.Megaparsec.Char       (char, eol)
import           Text.Megaparsec.Char.Lexer (decimal)

import qualified Data.IntMap                as M (empty)
import           Data.Sequence              (singleton)
import           Debug.Trace

type Bytes = IntSet

data Maze =
  Maze Low High Open Falling

type Low = Int

type High = Int

type Open = IntSet

type Falling = [Int]

parseInput :: Parser [Int]
parseInput = manyTill parseByte eof

parseByte :: Parser Int
parseByte = do
  x <- decimal
  char ','
  y <- decimal
  eol
  return (x + shiftL y 7)

origin = 0

goal test
  | test = 6 + shiftL 6 7
  | otherwise = 70 + shiftL 70 7

range test
  | test = (0, 6)
  | otherwise = (0, 70)

dirs = [-1, 1, -128, 128]

inRange :: (Int, Int) -> Int -> Bool
inRange (mini, maxi) int = mv >= mini && mV <= maxi
  where
    mv = min (int .&. 127) (int `shiftR` 7)
    mV = max (int .&. 127) (int `shiftR` 7)

intToPos :: Int -> String
intToPos int = show x ++ "," ++ show y
  where
    x = int .&. 127
    y = int `shiftR` 7

shortestPath :: Bool -> Bytes -> Int
shortestPath test bytes =
  fromJust . bfsSafeDist origin (neighbours test bytes) $ (== goal test)

neighbours :: Bool -> Bytes -> Int -> [Int]
neighbours test bytes pos =
  filter (\p -> inRange (range test) p && p `notMember` bytes) . map (pos +)
    $ [-1, 1, -128, 128]

neighboursB :: Bool -> Bytes -> Int -> [Int]
neighboursB test open pos =
  filter (\p -> inRange (range test) p && p `member` open) . map (pos +) $ dirs

binary :: Bool -> [Int] -> Int
binary test falling = evalState (binaryState test) (Maze low high open later)
  where
    (incoming, later) = splitAt 1024 falling
    low = 1024
    high = 2 ^ (ceiling . logBase 2 . fromIntegral . length $ falling)
    open = dfs [origin] (neighbours test (fromList incoming)) empty

binaryState :: Bool -> State Maze Int
binaryState test = do
  (Maze low high open falling) <- get
  let mid = div (high - low) 2
      (incoming, later) = splitAt mid falling
      incoming' = fromList incoming
      open' = difference open incoming'
      closed = intersection open incoming'
      midVal = low + mid
      midPaths = dfs [origin] (neighboursB test open') empty
      low'
        | S.null closed = midVal
        | S.null open' || goal test `notMember` midPaths = low
        | otherwise = midVal
      (high', falling', open'')
        | S.null closed = (high, later, open)
        | low' == midVal = (high, later, midPaths)
        | otherwise = (midVal, incoming, open)
      result
        | high == low + 1 = return (head falling)
        | otherwise = do
          put (Maze low' high' open'' falling')
          binaryState test
  result

part1 :: Bool -> Text -> String
part1 test =
  show
    . shortestPath test
    . fromList
    . take number
    . fromRight (error "parser failed")
    . parse parseInput "day18"
  where
    number
      | test = 12
      | otherwise = 1024

part2 :: Bool -> Text -> String
part2 test =
  intToPos
    . binary test
    . fromRight (error "parser failed")
    . parse parseInput "day18"
