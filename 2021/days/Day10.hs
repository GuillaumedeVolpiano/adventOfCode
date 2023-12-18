module Day10
  ( part1
  , part2
  ) where

import           Data.List  (sort)
import           Data.Map   (fromList, (!))
import           Data.Maybe (Maybe (Just, Nothing), isNothing, mapMaybe)

closing = fromList [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

errorScore = fromList [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

incompleteScore = fromList [('(', 1), ('[', 2), ('{', 3), ('<', 4)]

lineFold :: Maybe String -> Char -> Maybe String
lineFold Nothing _ = Nothing
lineFold (Just "") a
  | a `elem` "([{<" = Just [a]
  | otherwise = Nothing
lineFold (Just s@(f:ss)) a
  | a `elem` "([{<" = Just (a : s)
  | closing ! f == a = Just ss
  | otherwise = Nothing

scoreErrorLine :: String -> String -> Int
scoreErrorLine _ [] = 0
scoreErrorLine "" (a:as)
  | a `elem` "([{<" = scoreErrorLine [a] as
  | otherwise = errorScore ! a
scoreErrorLine s@(f:ss) (a:as)
  | a `elem` "([{<" = scoreErrorLine (a : s) as
  | closing ! f == a = scoreErrorLine ss as
  | otherwise = errorScore ! a

scoreCorrect :: String -> Int
scoreCorrect []     = 0
scoreCorrect (a:as) = incompleteScore ! a + 5 * scoreCorrect as

middle :: [a] -> a
middle l = l !! div (length l) 2

part1 :: Bool -> String -> String
part1 _ = show . sum . map (scoreErrorLine "") . lines

part2 :: Bool -> String -> String
part2 _ =
  show .
  middle .
  sort .
  map (scoreCorrect . reverse) . mapMaybe (foldl lineFold (Just "")) . lines
