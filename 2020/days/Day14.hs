module Day14
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Bits            ((.&.), (.|.))
import           Data.Char            (digitToInt, isAlphaNum, isDigit)
import           Data.IntMap.Strict   (IntMap, elems, empty, insert)
import           Data.List.Split      (splitOn)
import           Helpers.Parsers      (Parser, parseByLine)
import           Text.Megaparsec      (takeWhile1P, try, (<|>))
import           Text.Megaparsec.Char (eol, string)

type Memory = IntMap Int

type Mask = Int

data Machine =
  Machine
    { memory    :: Memory
    , andMask   :: Mask
    , orMask    :: Mask
    , multiMask :: [(Mask, Mask)]
    }
  deriving (Show)

parser :: Parser (Machine -> Machine)
parser = try mask <|> mem
  where
    mask = do
      void $ string "mask = "
      t <- takeWhile1P Nothing isAlphaNum
      void eol
      return (createMask t)
    mem = do
      void $ string "mem["
      index <- takeWhile1P Nothing isDigit
      void $ string "] = "
      val <- takeWhile1P Nothing isDigit
      void eol
      return (sendToMem (read index) (read val))

secondParser :: Parser (Machine -> Machine)
secondParser = try mask <|> mem
  where
    mask = do
      void $ string "mask = "
      t <- takeWhile1P Nothing isAlphaNum
      void eol
      return (createMultiMask t)
    mem = do
      void $ string "mem["
      index <- takeWhile1P Nothing isDigit
      void $ string "] = "
      val <- takeWhile1P Nothing isDigit
      void eol
      return (sendToMultiMem (read index) (read val))

createMultiMask :: String -> Machine -> Machine
createMultiMask newMasks machine =
  machine {multiMask = map fromBin . multi $ newMasks}
  where
    multi = foldr makeMulti [([], [])]
    makeMulti 'X' r = map (bival 0) r ++ map (bival 1) r
    makeMulti c r   = map (add (digitToInt c)) r
    add x (om, am) = (x : om, 1 : am)
    bival x (om, am) = (x : om, x : am)
    fromBin (a, b) = (unBin a, unBin b)
    unBin = foldl (\a b -> b + 2 * a) 0

sendToMultiMem :: Int -> Int -> Machine -> Machine
sendToMultiMem index val machine = machine {memory = newMemory}
  where
    indices = map (\(om, am) -> (.&.) am . (.|.) om $ index) (multiMask machine)
    newMemory = foldl (\a b -> insert b val a) (memory machine) indices

createMask :: String -> Machine -> Machine
createMask newMask machine = machine {andMask = am, orMask = om}
  where
    (om, am) = toMask newMask

sendToMem :: Int -> Int -> Machine -> Machine
sendToMem index val machine =
  machine {memory = insert index (unmask machine val) (memory machine)}

unmask :: Machine -> Int -> Int
unmask machine = (.&.) am . (.|.) om
  where
    am = andMask machine
    om = orMask machine

toMask :: String -> (Mask, Mask)
toMask string =
  ( foldl
      (\a x ->
         if x == '1'
           then 1 + 2 * a
           else 2 * a)
      0
      string
  , foldl
      (\a x ->
         if x == '0'
           then 2 * a
           else 1 + 2 * a)
      0
      string)

part1 :: Bool -> String -> String
part1 _ =
  show .
  sum .
  elems .
  memory . foldl (\a b -> b a) (Machine empty 0 0 []) . parseByLine parser

part2 :: Bool -> String -> String
part2 _ =
  show .
  sum .
  elems .
  memory . foldl (\a b -> b a) (Machine empty 0 0 []) . parseByLine secondParser
