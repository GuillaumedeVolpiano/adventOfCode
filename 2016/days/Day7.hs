module Day7
  ( part1
  , part2
  ) where

import           Data.List       (intersect)
import           Data.List.Split (splitOneOf)

hasTLS :: String -> Bool
hasTLS ip = any isABBA external && not (any isABBA internal)
  where
    ips = splitOneOf "[]" ip
    (external, internal) = foldr alternate ([], []) ips

alternate :: a -> ([a], [a]) -> ([a], [a])
alternate x (others, ones) = (x : ones, others)

isABBA :: String -> Bool
isABBA xs
  | length xs < 4 = False
isABBA (a:b:c:d:xs) = (a == d && b == c && a /= b) || isABBA (b : c : d : xs)

hasSSL :: String -> Bool
hasSSL ip =
  not . null . intersect (concatMap listABA external)
    $ concatMap listBAB internal
  where
    ips = splitOneOf "[]" ip
    (external, internal) = foldr alternate ([], []) ips

listABA :: String -> [(Char, Char)]
listABA xs
  | length xs < 3 = []
listABA (a:b:c:xs)
  | a == c && a /= b = (a, b) : listABA (b : c : xs)
  | otherwise = listABA (b : c : xs)

listBAB :: String -> [(Char, Char)]
listBAB xs
  | length xs < 3 = []
listBAB (a:b:c:xs)
  | a == c && a /= b = (b, a) : listBAB (b : c : xs)
  | otherwise = listBAB (b : c : xs)

part1 :: Bool -> String -> String
part1 _ = show . length . filter hasTLS . lines

part2 :: Bool -> String -> String
part2 _ = show . length . filter hasSSL . lines
