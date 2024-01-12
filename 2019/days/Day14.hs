module Day14
  ( part1
  , part2
  ) where

import           Control.Monad        (void)
import           Data.Bifunctor       (second)
import           Data.Char            (isAlpha, isDigit)
import           Data.Either          (fromRight)
import           Data.HashMap.Lazy    as M (HashMap, alter, empty, filter,
                                            findWithDefault, fromList, insert,
                                            keys, member, singleton, size, (!))
import           Data.List            as L (filter)
import           Helpers.Parsers      (Parser)
import           Text.Megaparsec      (many, optional, parse, someTill,
                                       takeWhile1P)
import           Text.Megaparsec.Char (char, eol, string)

type Reactions = HashMap String Input

type Output = HashMap String Int

type Input = [(String, Int)]

type Required = HashMap String Int

cargo = 1000000000000

parser :: Parser (Output, Reactions)
parser =
  (\a -> (fromList . map (second fst) $ a, fromList . map (second snd) $ a)) <$>
  many parseLine

parseLine :: Parser (String, (Int, Input))
parseLine = do
  dest <- someTill parseReagent (string "=> ")
  (product, val) <- parseReagent
  void . optional $ eol
  return (product, (val, dest))

parseReagent :: Parser (String, Int)
parseReagent = do
  num <- read <$> takeWhile1P Nothing isDigit
  void . char $ ' '
  reagent <- takeWhile1P Nothing isAlpha
  void . optional . char $ ','
  void . optional . char $ ' '
  void . optional $ eol
  return (reagent, num)

produce :: Reactions -> Output -> Required -> Required
produce reactions output required
  | size needs == 1 && "ORE" `member` needs = required
  | otherwise = produce reactions output newRequired
  where
    needs = M.filter (> 0) required
    next = head . L.filter (/= "ORE") . keys $ needs
    needed = required ! next
    out = output ! next
    batches
      | mod needed out == 0 = div needed out
      | otherwise = div needed out + 1
    leftover
      | mod needed out == 0 = 0
      | otherwise = mod needed out - out
    newNeeds = reactions ! next
    newRequired =
      foldr
        (\(ingredient, qty) req -> alter (amend (batches * qty)) ingredient req)
        (insert next leftover required)
        newNeeds
    amend nval Nothing     = Just nval
    amend nval (Just aval) = Just $ aval + nval

produceFuel :: Int -> (Output, Reactions) -> Int
produceFuel quantity (output, reactions) = produced ! "ORE"
  where
    produced = produce reactions output $ singleton "FUEL" quantity

searchProduction :: Int -> Int -> (Output, Reactions) -> Int
searchProduction minVal maxVal or
  | maxVal - minVal == 1 = minVal
  | midProd == cargo = midVal
  | produceFuel maxVal or < cargo = searchProduction maxVal (2 * maxVal) or
  | otherwise = searchProduction nMin nMax or
  where
    midVal = div (maxVal + minVal) 2
    midProd = produceFuel midVal or
    nMin
      | midProd < cargo = midVal
      | otherwise = minVal
    nMax
      | nMin == midVal = maxVal
      | otherwise = midVal

findMaxProd :: (Output, Reactions) -> Int
findMaxProd or = searchProduction minVal maxVal or
  where
    minVal = div cargo . produceFuel 1 $ or
    maxVal = cargo

part1 :: Bool -> String -> String
part1 _ = show . produceFuel 1 . fromRight (empty, empty) . parse parser ""

part2 :: Bool -> String -> String
part2 _ = show . findMaxProd . fromRight (empty, empty) . parse parser ""
