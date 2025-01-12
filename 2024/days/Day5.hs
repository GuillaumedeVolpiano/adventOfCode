module Day5
  ( part1
  , part2
  ) where

import           Data.ByteString            (ByteString)
import           Data.Either                (fromRight)
import           Data.Function              (on)
import           Data.IntMap                (IntMap, empty, fromList, notMember,
                                             (!))
import           Data.List                  (groupBy, sortBy)
import           Data.Ord                   (comparing)
import           Data.Word8                 (_bar, _comma)
import           Helpers.Parsers.ByteString (Parser)
import           Text.Megaparsec            (eof, manyTill, optional, parse,
                                             sepBy)
import           Text.Megaparsec.Byte       (char, eol)
import           Text.Megaparsec.Byte.Lexer (decimal)

type Rules = IntMap [Int]

type Update = [Int]

parseInput :: Parser (Rules, [Update])
parseInput = do
  rules <- buildRules <$> manyTill parseOrder eol
  updates <- manyTill parseUpdate eof
  return (rules, updates)

parseOrder :: Parser (Int, Int)
parseOrder = do
  before <- decimal
  char _bar
  after <- decimal
  eol
  return (after, before)

parseUpdate :: Parser [Int]
parseUpdate = do
  update <- decimal `sepBy` char _comma
  optional eol
  return update

buildRules :: [(Int, Int)] -> Rules
buildRules =
  fromList
    . map (foldr construct (0, []))
    . groupBy ((==) `on` fst)
    . sortBy (comparing fst)
  where
    construct (a, b) (_, xs) = (a, b : xs)

sorted :: Rules -> Update -> Bool
sorted _ [] = True
sorted rules (x:xs)
  | x `notMember` rules = sorted rules xs
  | any (`elem` xs) . (!) rules $ x = False
  | otherwise = sorted rules xs

checkSorted :: (Rules, [Update]) -> [Update]
checkSorted (rules, updates) = filter (sorted rules) updates

comparePages :: Rules -> Int -> Int -> Ordering
comparePages rules page1 page2
  | page1 == page2 = EQ
  | page2 `notMember` rules = GT
  | page1 `elem` rules ! page2 = LT
  | otherwise = GT

sortUnsorted :: (Rules, [Update]) -> [Update]
sortUnsorted (rules, updates) =
  map (sortBy (comparePages rules)) . filter (not . sorted rules) $ updates

score :: Update -> Int
score update = (update !!) . flip div 2 . length $ update

part1 :: Bool -> ByteString -> String
part1 _ =
  show
    . sum
    . map score
    . checkSorted
    . fromRight (empty, [])
    . parse parseInput ""

part2 :: Bool -> ByteString -> String
part2 _ =
  show
    . sum
    . map score
    . sortUnsorted
    . fromRight (empty, [])
    . parse parseInput ""
