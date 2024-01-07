module Day19
  ( part1
  , part2
  ) where

import           Control.Monad                (guard, void)
import           Data.Char                    (isDigit)
import           Data.Either                  (fromRight, isRight)
import           Data.List.Split              (splitWhen)
import           Data.Map                     (Map, fromList, (!))
import           Helpers.Parsers              (Parser)
import           Text.ParserCombinators.ReadP (ReadP, char, choice, eof, get,
                                               many, many1, manyTill, optional,
                                               readP_to_S, satisfy, sepBy,
                                               string, (+++))

type ParserBuilder = Map Int [String]

parse :: ReadP a -> String -> a
parse parser = fst . head . filter ((== "") . snd) . readP_to_S parser

parseBuildLine :: ReadP (Int, [String])
parseBuildLine =
  (,) <$> ident <* string ": " <*> choice [numbers, manyNumbers, findChar]
  where
    ident = read <$> aNumber
    numbers = (++) <$> bl <*> fmap (["|"] ++) manyNumbers
    bl = sepBy aNumber (char ' ') <* string " | "
    manyNumbers = sepBy aNumber (char ' ')
    aNumber = many1 (satisfy isDigit)
    findChar = fmap (\x -> [[x]]) $ string "\"" *> get <* string "\""

buildParser :: Int -> ParserBuilder -> ReadP ()
buildParser rule builder
  | dest == ["a"] = void . char $ 'a'
  | dest == ["b"] = void . char $ 'b'
  | "|" `elem` dest = createParser a +++ createParser b
  | otherwise = createParser dest
  where
    dest = builder ! rule
    [a, b] = splitWhen (== "|") dest
    createParser = foldl1 chain . map (\x -> buildParser (read x) builder)
    chain a b = do
      a
      b

countParse :: ReadP a -> [String] -> Int
countParse parser =
  length . concatMap (filter ((== "") . snd) . readP_to_S parser)

testMessages :: [[String]] -> Int
testMessages [a, b] = countParse (parser <* eof) b
  where
    parser = buildParser 0 . fromList . map (parse parseBuildLine) $ a

buildInfiniteParser :: ParserBuilder -> ReadP ()
buildInfiniteParser builder = do
  fourtyTwo
  r1 <- many1 fourtyTwo
  r2 <- many1 thirtyOne
  eof
  guard $ length r1 >= length r2
  where
    fourtyTwo = buildParser 42 builder
    thirtyOne = buildParser 31 builder

testInfiniteMessages :: [[String]] -> Int
testInfiniteMessages [a, b] = countParse parser b
  where
    parser = buildInfiniteParser . fromList . map (parse parseBuildLine) $ a

part1 :: Bool -> String -> String
part1 _ = show . testMessages . splitWhen null . lines

part2 :: Bool -> String -> String
part2 _ = show . testInfiniteMessages . splitWhen null . lines
