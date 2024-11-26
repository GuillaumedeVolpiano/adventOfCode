module Day16
  ( part1
  , part2
  ) where

import           Data.Either          (fromRight)
import           Data.Sequence        as S (Seq, elemIndexL, empty, fromList,
                                            index, length, splitAt, update,
                                            (><))
import           Helpers.Parsers      (Parser, nums)
import           Helpers.Search       (findPattern)
import           Text.Megaparsec      (optional, parse, (<|>))
import           Text.Megaparsec.Char (char, eol, lowerChar, string)

dance test
  | test = fromList ['a' .. 'e']
  | otherwise = fromList ['a' .. 'p']

type Dance = Seq Char

parseInput :: Parser (Dance -> Dance)
parseInput = spin <|> exchange <|> partner <|> finish

spin :: Parser (Dance -> Dance)
spin = do
  char 's'
  Just a <- nums
  optional . char $ ','
  (. splitted a) <$> parseInput

splitted :: Int -> Dance -> Dance
splitted a dance = after >< before
  where
    splitPoint = S.length dance - a
    (before, after) = S.splitAt splitPoint dance

exchange :: Parser (Dance -> Dance)
exchange = do
  char 'x'
  Just a <- nums
  char '/'
  Just b <- nums
  optional . char $ ','
  (. exchanged a b) <$> parseInput

exchanged :: Int -> Int -> Dance -> Dance
exchanged a b dance = update a to . update b from $ dance
  where
    from = index dance a
    to = index dance b

partner :: Parser (Dance -> Dance)
partner = do
  char 'p'
  a <- lowerChar
  char '/'
  b <- lowerChar
  optional . char $ ','
  (. partnered a b) <$> parseInput

partnered :: Char -> Char -> Dance -> Dance
partnered a b dance = update indexA b . update indexB a $ dance
  where
    (Just indexA) = elemIndexL a dance
    (Just indexB) = elemIndexL b dance

finish :: Parser (Dance -> Dance)
finish = do
  eol
  return id

makeDance :: String -> (Dance -> Dance)
makeDance = fromRight id . parse parseInput ""

findResult :: Bool -> Int -> (Dance -> Dance) -> String
findResult test point steps = foldr (:) [] $ shortList !! patterned
  where
    shortList = take 150 . iterate steps . dance $ test
    pat = findPattern 0 10 (==) . fromList $ shortList
    patterned = point `mod` pat

part1 :: Bool -> String -> String
part1 test input = foldr (:) [] . steps . dance $ test
  where
    steps = makeDance input

part2 :: Bool -> String -> String
part2 test = findResult test (10 ^ 9) . makeDance
